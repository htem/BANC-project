#!/usr/bin/env python3
"""Fix linked file paths in Illustrator .ai files while maintaining PDF validity.

.ai files are PDF-compatible. Linked paths appear in:
  1. XMP metadata stream (stRef:filePath XML tags)
  2. PostScript private data (%%DocumentFiles, %AI5_File)

Changing string lengths requires:
  - Updating /Length values for any affected PDF streams
  - Rebuilding the PDF xref table with corrected byte offsets
"""

import re
import sys
import shutil
import os


def fix_ai_paths(filepath, replacements, backup=True, dry_run=False):
    """Fix linked file paths in an .ai file.

    Args:
        filepath: Path to .ai file
        replacements: List of (old_string, new_string) tuples (as regular strings)
        backup: Whether to create a .bak backup
        dry_run: If True, only report what would change
    Returns:
        dict with 'changed': bool, 'counts': {old: count}
    """
    with open(filepath, 'rb') as f:
        data = f.read()

    original_size = len(data)
    counts = {}

    # Count occurrences before replacing
    for old_str, new_str in replacements:
        old_bytes = old_str.encode('utf-8') if isinstance(old_str, str) else old_str
        count = data.count(old_bytes)
        if count > 0:
            counts[old_str] = count

    if not counts:
        print(f"  No matching paths found in {os.path.basename(filepath)}")
        return {'changed': False, 'counts': counts}

    if dry_run:
        print(f"  {os.path.basename(filepath)}:")
        for old_str, cnt in counts.items():
            new_str = next(n for o, n in replacements if o == old_str)
            print(f"    {cnt}x: {old_str!r} -> {new_str!r}")
        return {'changed': False, 'counts': counts}

    # Backup
    if backup:
        bak_path = filepath + '.bak'
        if not os.path.exists(bak_path):
            shutil.copy2(filepath, bak_path)
            print(f"  Backup: {os.path.basename(bak_path)}")

    # Step 1: Make all string replacements
    for old_str, new_str in replacements:
        old_bytes = old_str.encode('utf-8') if isinstance(old_str, str) else old_str
        new_bytes = new_str.encode('utf-8') if isinstance(new_str, str) else new_str
        data = data.replace(old_bytes, new_bytes)

    size_delta = len(data) - original_size
    if size_delta == 0:
        # Same-length replacement — no structural repair needed
        with open(filepath, 'wb') as f:
            f.write(data)
        print(f"  Same-length replacement, no PDF repair needed")
        for old_str, cnt in counts.items():
            print(f"    {cnt}x: {old_str!r}")
        return {'changed': True, 'counts': counts}

    # Step 2: Fix /Length values for PDF streams whose content changed
    # Find all PDF stream boundaries
    # Pattern: /Length DIGITS ... >>stream\r\n ... endstream
    length_fixes = 0

    # We iterate over all stream markers and check/fix their /Length
    stream_pattern = re.compile(rb'stream\r?\n')
    endstream_pattern = re.compile(rb'(?:\r\n|\r|\n)endstream')

    pos = 0
    while pos < len(data):
        # Find next stream start
        sm = stream_pattern.search(data, pos)
        if not sm:
            break

        content_start = sm.end()

        # Find corresponding endstream
        em = endstream_pattern.search(data, content_start)
        if not em:
            pos = content_start
            continue

        actual_length = em.start() - content_start

        # Find the /Length declaration before this stream marker
        # Search backwards from stream marker for /Length DIGITS
        search_start = max(0, sm.start() - 500)
        header = data[search_start:sm.start()]
        lm = list(re.finditer(rb'/Length\s+(\d+)', header))

        if lm:
            last_match = lm[-1]
            declared_length = int(last_match.group(1))

            if declared_length != actual_length:
                # Need to fix this /Length value
                old_length_str = last_match.group(1)
                new_length_str = str(actual_length).encode()

                # Pad with leading spaces to maintain same byte count
                if len(new_length_str) < len(old_length_str):
                    new_length_str = b' ' * (len(old_length_str) - len(new_length_str)) + new_length_str
                elif len(new_length_str) > len(old_length_str):
                    # Length digits increased — this is extremely unlikely for our path shortenings
                    # but handle it: just replace directly and accept the 1-byte shift
                    pass

                abs_start = search_start + last_match.start(1)
                abs_end = search_start + last_match.end(1)

                data = data[:abs_start] + new_length_str + data[abs_end:]
                length_fixes += 1

        pos = em.end()

    if length_fixes:
        print(f"  Fixed {length_fixes} PDF stream /Length value(s)")

    # Step 3: Rebuild the xref table
    # Find the existing xref section
    xref_pos = data.rfind(b'\nxref\n')
    if xref_pos < 0:
        xref_pos = data.rfind(b'\rxref\r')
    if xref_pos < 0:
        xref_pos = data.rfind(b'\nxref\r')
    if xref_pos < 0:
        xref_pos = data.rfind(b'\rxref\n')

    if xref_pos < 0:
        print(f"  WARNING: No xref table found — file may use xref streams (not supported)")
        with open(filepath, 'wb') as f:
            f.write(data)
        return {'changed': True, 'counts': counts}

    xref_pos += 1  # skip the leading \n or \r

    # Find startxref
    startxref_match = re.search(rb'startxref\r?\n?(\d+)', data[xref_pos:])
    if not startxref_match:
        print(f"  WARNING: No startxref found")
        with open(filepath, 'wb') as f:
            f.write(data)
        return {'changed': True, 'counts': counts}

    # Parse the existing xref table to get the object count
    # Format: xref\n0 N\n followed by N entries of 20 bytes each
    xref_header_match = re.match(rb'xref\r?\n(\d+)\s+(\d+)\r?\n', data[xref_pos:])
    if not xref_header_match:
        print(f"  WARNING: Cannot parse xref header")
        with open(filepath, 'wb') as f:
            f.write(data)
        return {'changed': True, 'counts': counts}

    first_obj = int(xref_header_match.group(1))
    obj_count = int(xref_header_match.group(2))

    # Find all PDF objects by scanning for "N M obj" markers
    # (only in the content before the xref table)
    content_data = data[:xref_pos]
    obj_offsets = {}

    for m in re.finditer(rb'(?:^|\r\n|\r|\n)(\d+)\s+(\d+)\s+obj(?:\r|\n| )', content_data):
        obj_num = int(m.group(1))
        gen_num = int(m.group(2))
        # The offset should point to the start of "N M obj", not the newline before
        offset = m.start()
        # Skip leading newline
        if content_data[offset:offset+1] in (b'\r', b'\n'):
            offset += 1
            if offset < len(content_data) and content_data[offset:offset+1] == b'\n':
                offset += 1  # skip \r\n
        obj_offsets[obj_num] = (offset, gen_num)

    # Build new xref table
    # Keep the same structure: single section starting from first_obj with obj_count entries
    new_xref_lines = [f"xref\r\n{first_obj} {obj_count}\r\n".encode()]

    for i in range(first_obj, first_obj + obj_count):
        if i == 0:
            # Object 0 is always free
            new_xref_lines.append(b"0000000000 65535 f \r\n")
        elif i in obj_offsets:
            offset, gen = obj_offsets[i]
            new_xref_lines.append(f"{offset:010d} {gen:05d} n \r\n".encode())
        else:
            # Object not found — keep as free entry
            new_xref_lines.append(b"0000000000 00000 f \r\n")

    new_xref = b"".join(new_xref_lines)

    # Find trailer
    trailer_match = re.search(rb'trailer\r?\n?<', data[xref_pos:])
    if not trailer_match:
        print(f"  WARNING: Cannot find trailer")
        with open(filepath, 'wb') as f:
            f.write(data)
        return {'changed': True, 'counts': counts}

    trailer_start = xref_pos + trailer_match.start()
    # Find the end: %%EOF
    eof_match = re.search(rb'%%EOF\r?\n?$', data[trailer_start:])
    if not eof_match:
        eof_match = re.search(rb'%%EOF', data[trailer_start:])

    trailer_data = data[trailer_start:]
    # Extract just trailer dict and %%EOF
    trailer_end_match = re.search(rb'>>\r?\n?startxref', trailer_data)
    if trailer_end_match:
        trailer_dict = trailer_data[:trailer_end_match.end()]
    else:
        # Fallback: grab everything up to %%EOF
        trailer_dict = trailer_data

    # Build the new tail: xref + trailer + startxref + %%EOF
    new_xref_offset = xref_pos
    data_before_xref = data[:xref_pos]

    # Reconstruct trailer section
    # Extract the trailer dictionary
    trailer_dict_match = re.search(rb'trailer\r?\n?(<.*?>>)', data[trailer_start:], re.DOTALL)
    if trailer_dict_match:
        trailer_dict_str = trailer_dict_match.group(1)
    else:
        trailer_dict_str = b"<</Size " + str(obj_count).encode() + b">>"

    new_tail = (
        new_xref
        + b"trailer\r"
        + trailer_dict_str
        + b"\r"
        + b"startxref\r"
        + str(new_xref_offset).encode()
        + b"\r"
        + b"%%EOF\r"
    )

    final_data = data_before_xref + new_tail

    with open(filepath, 'wb') as f:
        f.write(final_data)

    new_size = len(final_data)
    print(f"  Size: {original_size:,} -> {new_size:,} ({new_size - original_size:+,} bytes)")
    print(f"  Rebuilt xref table ({obj_count} objects)")
    for old_str, cnt in counts.items():
        new_str = next(n for o, n in replacements if o == old_str)
        print(f"    {cnt}x: {old_str!r} -> {new_str!r}")

    return {'changed': True, 'counts': counts}


def main():
    base = "/Users/papers/BANC-project/figures"

    # Define all path replacements
    # Canonical path: /Users/papers/BANC-project/figures/
    replacements_projects = [
        ("/Users/Projects/BANC-project/", "/Users/papers/BANC-project/"),
    ]
    replacements_hyang = [
        ("/Users/hyang/HMS Dropbox/Helen Yang/BANC-project/", "/Users/papers/BANC-project/"),
    ]
    replacements_abates = [
        ("/Users/abates/HMS Dropbox/Alexander Bates/neuroanat/BANC-project/", "/Users/papers/BANC-project/"),
    ]
    replacements_case = [
        ("figures/figure_2/Links/", "figures/figure_2/links/"),
    ]

    # Map each .ai file to its needed replacements
    fixes = [
        ("figure_1/figure_1.ai", replacements_projects),
        ("figure_2/figure_2.ai", replacements_case),
        ("figure_1/extended_data_figure_1.ai", replacements_hyang),
        ("figure_2/extended_data_figure_4.ai", replacements_hyang),
        ("figure_2/extended_data_figure_6.ai", replacements_abates),
        ("figure_3/figure_3.ai", replacements_hyang),
        ("figure_3/extended_data_figure_7.ai", replacements_hyang),
        ("figure_4/extended_data_figure_8.ai", replacements_hyang),
        ("figure_6/figure_6.ai", replacements_hyang),
    ]

    dry_run = "--dry-run" in sys.argv

    if dry_run:
        print("=== DRY RUN — no files will be modified ===\n")
    else:
        print("=== Fixing .ai file paths ===\n")

    for rel_path, repls in fixes:
        filepath = os.path.join(base, rel_path)
        if not os.path.exists(filepath):
            print(f"  SKIP (not found): {rel_path}")
            continue
        print(f"\n{rel_path}:")
        fix_ai_paths(filepath, repls, backup=True, dry_run=dry_run)

    print("\nDone.")


if __name__ == "__main__":
    main()
