# BANC Specimen Information

This document provides detailed information about the specimen used to generate the BANC (Brain-And-Nerve-Cord) connectome dataset.

## Project Leadership

This phase of the project was led by [Minsu Kim](https://www.linkedin.com/in/mindy-minsu-kim/). Generating this beautiful sample was a heroic effort by Minsu, done largely during the lockdown/isolation phase of the COVID-19 pandemic.

## Fly Information

- **Species:** *Drosophila melanogaster*
- **Genotype:** Progeny of a cross between a female Canton-S fly and a male w1118 fly
- **Sex:** Female
- **Age:** 5-6 days post eclosion (117-146 hrs) 
  - **Eclosion date:** Between October 20th 5pm and October 21st 5pm, 2020
  - **Dissection date:** October 26th, 2020, between 2pm and 7pm
- **Rearing:** Raised at room temperature. Raised in a vial with other flies until Oct 25th ~3pm, 2020 (4-5 days post eclosion) when it was run through a behavioural assay and then subsequently housed individually for ~1 day until dissection.

## Behavioural Information

- **Fly ID number:** T2F73
- [TODO more details]

## Fixation, Staining, and Embedding Protocol

The fly was pinned onto a dissection pad, ventral-side up. The fly was then submerged in a drop of ice cold Karnovsky's fixative (2.5% formaldehyde, 2.5% glutaraldehyde in 0.1M cacodylate buffer, pH 7.4) containing 0.04% CaCl₂. First, its legs and proboscis were removed to allow fixative to access the nervous tissue. Next, its head capsule and cuticle of the ventral thorax were carefully removed to expose the nervous tissue for extraction. Within 5 minutes, the brain and connected VNC were completely dissected and transferred to an Eppendorf tube containing the same Karnovsky's fixative. The sample was then kept at 4 degrees C overnight. 

On the subsequent day, the sample was washed with 0.02M 3-amino-1,2,4-triazole (A-TRA) in cacodylate buffer (3×10min) and then stained with 1% OsO₄ in 0.1M A-TRA for 90min on ice. On the same day, the sample was stained with 1% thiocarbohydrazide for 8min at 40 degrees C, 2% unbuffered OsO₄ at room temperature for 60 minutes, and 1% uranyl acetate in maleate buffer at 4 degrees C overnight. 

On the next day, the sample was stained with lead aspartate for 3 hours at 60 degrees C and was then dehydrated in a graded ethanol series, washed with propylene oxide, and infiltrated with 2:1 and 1:2 propylene oxide:LX-112 resin consecutively for 30 minutes each. The sample was then placed in pure LX-112 resin overnight at 4 degrees and was embedded in fresh pure resin on the following day. The resin block was cured at 60 degrees C for 48 hours.

## X-ray Scanning

The resin-embedded nervous system sample was scanned on a Zeiss microCT X-ray scanner before serial sectioning. [View the raw X-ray data here](https://spelunker.cave-explorer.org/#!%7B%22dimensions%22:%7B%22x%22:%5B0.000001304%2C%22m%22%5D%2C%22y%22:%5B0.000001304%2C%22m%22%5D%2C%22z%22:%5B0.000001304%2C%22m%22%5D%7D%2C%22position%22:%5B515.5%2C537.5%2C432.5%5D%2C%22crossSectionOrientation%22:%5B-0.7071067690849304%2C0%2C0%2C0.7071067690849304%5D%2C%22crossSectionScale%22:2.0441866822585437%2C%22projectionOrientation%22:%5B-0.7850747108459473%2C-0.02853301726281643%2C0.008892238140106201%2C0.6186796426773071%5D%2C%22projectionScale%22:1589.9721917556074%2C%22layers%22:%5B%7B%22type%22:%22image%22%2C%22source%22:%22precomputed://gs://lee-lab_brain-and-nerve-cord-fly-connectome/microCT%22%2C%22tab%22:%22source%22%2C%22name%22:%22BANC%20sample%20microCT%22%7D%2C%7B%22type%22:%22segmentation%22%2C%22source%22:%22precomputed://gs://lee-lab_brain-and-nerve-cord-fly-connectome/microCT/meshes%22%2C%22tab%22:%22segments%22%2C%22segments%22:%5B%221%22%5D%2C%22segmentQuery%22:%221%22%2C%22name%22:%22mesh%22%7D%5D%2C%22showSlices%22:false%2C%22selectedLayer%22:%7B%22visible%22:true%2C%22layer%22:%22BANC%20sample%20microCT%22%7D%2C%22layout%22:%224panel%22%7D). Below is a max intensity projection of the scan, oriented with the brain at the top and the ventral nerve cord at the bottom.

<img height=480 alt="The BANC microCT max projection" src=https://user-images.githubusercontent.com/23616964/190926530-699702b3-3beb-4b75-b2b2-397100254c34.png>

No obvious defects or damage could be seen in this sample from inspecting the X-ray scan. Critically, the neck connective appeared intact.