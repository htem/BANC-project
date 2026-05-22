# Dataverse upload workspace

Scripts, manifests and per-file documentation for populating the BANC v888
Harvard Dataverse:

- **New dataset (paper)** — doi:10.7910/DVN/7WTH1N
  https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/7WTH1N
- **Preprint dataset (reference only)** — doi:10.7910/DVN/8TFGGB
- **Budget** — 2 TB

## Layout

```
manuscript/print/dataverse/
├── README.md              ← you are here
├── documentation/         ← one .md per file we upload
│                            YAML frontmatter holds API metadata
│                            body holds human-readable docs
└── (later) upload.py      ← orchestration script
└── (later) manifest.csv   ← derived from documentation/*.md
```

## API key

Stored at `~/.dataverse_api_key` (chmod 600). NEVER commit; NEVER paste in
filenames, env files inside the repo, or tracked .md.

To use in shell:

```bash
export DATAVERSE_API_KEY=$(cat ~/.dataverse_api_key)
```

## Per-file workflow

For each file we want to upload:

1. Write `documentation/<basename>.md` with YAML frontmatter (API fields:
   `description`, `categories`, `directoryLabel`, `restrict`, `tabIngest`)
   and a body describing source, schema, provenance, related files.
2. User inspects and approves.
3. POST file + metadata to `/api/datasets/:persistentId/add` (one call).
4. Verify with `GET /api/datasets/:persistentId/?persistentId=...`.

The frontmatter fields are submitted as the `jsonData` part of the multipart
POST; the body of the .md is *not* sent to Dataverse — it is local
documentation that will also feed into the dataset-level `documentation.md`
written later (see step 2 of the master plan in
`manuscript/print/banc_data_locations.md`).

## API reference (Dataverse 6.x)

| call | purpose |
|---|---|
| `GET /api/users/:me` | auth check |
| `GET /api/datasets/:persistentId/?persistentId=…` | dataset state, files in latest version |
| `POST /api/datasets/:persistentId/add?persistentId=…` | upload bytes + per-file metadata in one multipart POST |
| `PUT /api/files/{id}/metadata` | update per-file metadata without re-uploading bytes |
| `DELETE /api/files/{id}` | remove a file from the current draft |
| `GET /api/datasets/:persistentId/uploadurls?persistentId=…&size=N` | request direct-S3 upload URL for large files |
| `POST /api/datasets/:persistentId/actions/:publish?persistentId=…&type=major` | publish current draft (irreversible without contacting curators) |

For files >100 MB use the direct-S3 upload route: GET an upload URL, PUT bytes
to S3 directly, then POST to `/add` with the returned `storageIdentifier`.

## Current state of the draft

Run:

```bash
curl -sS -H "X-Dataverse-key:$(cat ~/.dataverse_api_key)" \
  "https://dataverse.harvard.edu/api/datasets/:persistentId/?persistentId=doi:10.7910/DVN/7WTH1N" \
  | python3 -c "import sys,json;d=json.load(sys.stdin)['data']['latestVersion'];print(d['versionState']);[print(' -',f['directoryLabel']+'/'+f['dataFile']['filename'],f['dataFile']['filesize'],'B') for f in d['files']]"
```
