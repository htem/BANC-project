#!/usr/bin/env python3
"""
Mappings between neuropil names in the BANC, FAFB, MANC, and FANC.

FAFB abbreviations that the BANC doesn't have equivalents for:
- LA (I assume lamina)
- OCG (I assume ocellar ganglion/glomerulus)

Everything else has an equivalent across the relevant datasets,
though not always perfect one-to-one equivalence:
- Some datasets don't specify _L vs _R for some neuropils:
  - FAFB's central complex neuropils: EB, FB, NO, PB
  - FAFB's PRW and SAD
  - MANC's IntTct and LTct
- FANC's mVAC isn't split into T1/T2/T3, though Jasper is
  meaning to do this split in FANC.

"""

import pandas as pd

neuropil_aliases = pd.DataFrame({
    'CB_AL': ('AL', None, None),
    'CB_AMMC': ('AMMC', None, None),
    'CB_AOTU': ('AOTU', None, None),
    'CB_ATL': ('ATL', None, None),
    'CB_AVLP': ('AVLP', None, None),
    'CB_BU': ('BU', None, None),
    'CB_CAN': ('CAN', None, None),
    'CB_CRE': ('CRE', None, None),
    'CB_EB': ('EB', None, None),  # No _L or _R in FAFB
    'CB_EPA': ('EPA', None, None),
    'CB_FB': ('FB', None, None),  # No _L or _R in FAFB
    'CB_FLA': ('FLA', None, None),
    'CB_GA': ('GA', None, None),
    'CB_GNG': ('GNG', None, None),
    'CB_GOR': ('GOR', None, None),
    'CB_IB': ('IB', None, None),
    'CB_ICL': ('ICL', None, None),
    'CB_IPS': ('IPS', None, None),
    'CB_LAL': ('LAL', None, None),
    'CB_LH': ('LH', None, None),
    'CB_MB-CA': ('MB_CA', None, None),
    'CB_MB-ML': ('MB_ML', None, None),
    'CB_MB-PED': ('MB_PED', None, None),
    'CB_MB-VL': ('MB_VL', None, None),
    'CB_NO': ('NO', None, None),  # No _L or _R in FAFB
    'CB_PB': ('PB', None, None),  # No _L or _R in FAFB
    'CB_PLP': ('PLP', None, None),
    'CB_PRW': ('PRW', None, None),  # No _L or _R in FAFB
    'CB_PVLP': ('PVLP', None, None),
    'CB_SAD': ('SAD', None, None),  # No _L or _R in FAFB
    'CB_SCL': ('SCL', None, None),
    'CB_SIP': ('SIP', None, None),
    'CB_SLP': ('SLP', None, None),
    'CB_SMP': ('SMP', None, None),
    'CB_SPS': ('SPS', None, None),
    'CB_VES': ('VES', None, None),
    'CB_WED': ('WED', None, None),
    'OL_AME': ('AME', None, None),
    'OL_LOP': ('LOP', None, None),
    'OL_LO': ('LO', None, None),
    'OL_ME': ('ME', None, None),
    'VNC_AMNp': (None, 'Ov', 'AMNp'),
    'VNC_ANm': (None, 'ANm', 'ANm'),
    'VNC_HTct': (None, 'HTct(UTct-T3)', 'HTct'),
    'VNC_IntTct': (None, 'IntTct', 'IntTct'),  # No _L or _R in MANC
    'VNC_LTct': (None, 'LTct', 'LTct'),  # No _L or _R in MANC
    'VNC_NTct': (None, 'NTct(UTct-T1)', 'NTct'),
    'VNC_T1-ProNm': (None, 'LegNp(T1)', 'ProNm'),
    'VNC_T1-mVAC': (None, 'mVAC(T1)', None),  # No T1/T2/T3 split in FANC (yet)
    'VNC_T2-MesoNm': (None, 'LegNp(T2)', 'MesoNm'),
    'VNC_T2-mVAC': (None, 'mVAC(T2)', None),  # No T1/T2/T3 split in FANC (yet)
    'VNC_T3-MetaNm': (None, 'LegNp(T3)', 'MetaNm'),
    'VNC_T3-mVAC': (None, 'mVAC(T3)', None),  # No T1/T2/T3 split in FANC (yet)
    'VNC_WTct': (None, 'WTct(UTct-T2)', 'WTct'),
    'VNC_unassigned': (None, None, 'unassigned'),
    'brain_unassigned': (None, None, None),
    'neck_unassigned': (None, None, None),
}).T
neuropil_aliases.columns = ['FAFB', 'MANC', 'FANC']
