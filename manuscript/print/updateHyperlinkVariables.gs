/**
 * updateHyperlinkVariables.gs
 *
 * Walks the active Google Doc and replaces the visible text of every
 * hyperlink whose URL is `var/<identity>` (or `http://var/<identity>`,
 * `https://var/<identity>`) with the matching column-B value from the
 * `variables` tab of the paired sheet.
 *
 * Colour conventions on update:
 *   - Resolved successfully → set foreground to the default hyperlink
 *     blue (#1155CC). This is what restores a number to "blue" after the
 *     author has manually marked it (e.g. red for "stale", black for
 *     pasted text), once the value in the sheet matches the doc.
 *   - Identity not found in the sheet → set foreground to red (#CC0000)
 *     and leave the existing visible text in place, so unresolved
 *     placeholders are visually flagged.
 *
 * Sheet of record:
 *   ID:  set in VARIABLES_SHEET_ID below — paste the actual Drive ID
 *        before running. The repo copy of this script ships with
 *        VARIABLES_SHEET_ID redacted; the live bound Apps Script
 *        project inside the manuscript Doc holds the real value.
 *   Tab: variables
 *   Cols: A = identity, B = value
 *
 * Run via Extensions → Apps Script → updateHyperlinkVariables, or open
 * the bound Apps Script project (Tools → Script editor from inside the
 * Doc) and click Run.
 */

const VARIABLES_SHEET_ID = 'REPLACE_WITH_DRIVE_ID';  // see header docstring
const VARIABLES_TAB      = 'variables';

// Default Google Docs hyperlink blue and a flag colour for unresolved
// links. Keep these in hex (RGB) form — that's what Docs expects.
const COLOUR_RESOLVED   = '#1155CC';
const COLOUR_UNRESOLVED = '#CC0000';

// Regex matches the link URL forms the doc uses:
//   var/<identity>
//   http://var/<identity>
//   https://var/<identity>
const VAR_URL_RE = /^(?:https?:\/\/)?var\/([A-Za-z0-9_]+)$/;


function updateHyperlinkVariables() {
  const ss = SpreadsheetApp.openById(VARIABLES_SHEET_ID);
  const sheet = ss.getSheetByName(VARIABLES_TAB);
  if (!sheet) {
    throw new Error('Variables tab not found: ' + VARIABLES_TAB);
  }
  const data = sheet.getRange(1, 1, sheet.getLastRow(), 2).getValues();
  // Build identity -> value map. Skip the header row.
  const lookup = {};
  for (let i = 1; i < data.length; i++) {
    const id  = String(data[i][0]).trim();
    const val = data[i][1];
    if (id) lookup[id] = (val === '' || val === null || val === undefined)
                          ? null
                          : String(val);
  }

  const doc = DocumentApp.getActiveDocument();
  const sections = [doc.getBody(), doc.getHeader(), doc.getFooter()];
  const footnotes = doc.getFootnotes();
  for (const fn of footnotes) sections.push(fn.getFootnoteContents());

  let resolved   = 0;
  let unresolved = 0;
  let unchanged  = 0;

  for (const sec of sections) {
    if (!sec) continue;
    const result = walkSection_(sec, lookup);
    resolved   += result.resolved;
    unresolved += result.unresolved;
    unchanged  += result.unchanged;
  }

  const msg = 'updateHyperlinkVariables: ' +
              resolved + ' resolved (blue), ' +
              unresolved + ' unresolved (red), ' +
              unchanged + ' already up-to-date.';
  Logger.log(msg);
  try {
    DocumentApp.getUi().alert(msg);
  } catch (e) {
    // No UI available (e.g. triggered headless); log only.
  }
}


/**
 * Walk every text element inside a container element, find var/ hyperlinks,
 * and update them. Returns counts.
 */
function walkSection_(container, lookup) {
  const out = { resolved: 0, unresolved: 0, unchanged: 0 };
  // RangeElement search is the most reliable way to enumerate text nodes
  // across paragraphs, list items, and tables.
  let searchResult = container.findElement(DocumentApp.ElementType.TEXT);
  while (searchResult) {
    const text = searchResult.getElement().asText();
    updateLinksInText_(text, lookup, out);
    searchResult = container.findElement(DocumentApp.ElementType.TEXT, searchResult);
  }
  return out;
}


/**
 * For each maximal run of characters that shares a hyperlink URL matching
 * the var/ pattern, replace the visible text with the sheet value and
 * re-set the foreground colour. Mutating Text mid-iteration is tricky
 * because indices shift when the replacement length differs — we collect
 * spans first, then apply right-to-left so earlier offsets stay valid.
 */
function updateLinksInText_(text, lookup, out) {
  const n = text.getText().length;
  if (n === 0) return;

  const spans = [];           // {start, end, identity, oldText}
  let curStart = -1;
  let curUrl   = null;
  for (let i = 0; i < n; i++) {
    const url = text.getLinkUrl(i);
    if (url !== curUrl) {
      if (curStart >= 0 && curUrl) {
        const m = VAR_URL_RE.exec(curUrl);
        if (m) spans.push({ start: curStart, end: i - 1, identity: m[1] });
      }
      curStart = (url !== null) ? i : -1;
      curUrl   = url;
    }
  }
  if (curStart >= 0 && curUrl) {
    const m = VAR_URL_RE.exec(curUrl);
    if (m) spans.push({ start: curStart, end: n - 1, identity: m[1] });
  }

  // Apply right-to-left so earlier indices remain valid.
  for (let i = spans.length - 1; i >= 0; i--) {
    const span = spans[i];
    const oldText = text.getText().substring(span.start, span.end + 1);
    if (lookup.hasOwnProperty(span.identity) && lookup[span.identity] !== null) {
      const newText = String(lookup[span.identity]);
      if (newText === oldText) {
        // Already up-to-date — still re-assert blue, in case the author
        // had marked it red/black manually.
        text.setForegroundColor(span.start, span.end, COLOUR_RESOLVED);
        out.unchanged++;
      } else {
        text.deleteText(span.start, span.end);
        text.insertText(span.start, newText);
        const newEnd = span.start + newText.length - 1;
        // Re-apply the URL (insertText may not inherit the link in some
        // edge cases) and force the colour back to the default link blue.
        text.setLinkUrl(span.start, newEnd, text.getLinkUrl(span.start) || ('var/' + span.identity));
        text.setForegroundColor(span.start, newEnd, COLOUR_RESOLVED);
        out.resolved++;
      }
    } else {
      // Identity not in the sheet — leave the text but flag it red.
      text.setForegroundColor(span.start, span.end, COLOUR_UNRESOLVED);
      out.unresolved++;
    }
  }
}
