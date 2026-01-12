## Execute this Python script to merge each farm-specific bird biodiversity PDF with the region-specific PDFs in pdf'##
# Key inputs from this script were generated in the 'Outreach_farmers.R' script & the 'Create_pdfs.py' script
# Run 'python3 Python/02_Merge_pdfs.py' in terminal to run this script

import os
import glob
import pandas as pd
from PyPDF2 import PdfMerger

# Set paths
Outreach_directory = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Outreach/Outreach_farmers"
pdf_directory = f'{Outreach_directory}/Farm_specific_PDFs'
regional_directory = f'{Outreach_directory}/Fun_facts_regional_pdfs'
merged_directory = f'{Outreach_directory}/Merged_pdfs'

# Load Excel created in the 'Outreach_farmers.R' script
data_filepath = f'{Outreach_directory}/Excels/Farm_names_IDs.xlsx'
df = pd.read_excel(data_filepath)
df["Id_gcs"] = pd.to_numeric(df["Id_gcs"], errors="coerce").astype("Int64")

# Ensure merged directory exists
os.makedirs(merged_directory, exist_ok=True)

# List all PDF files in the directory
pdf_files = glob.glob(os.path.join(pdf_directory, "*.pdf"))

# Iterate over each PDF in the list
for pdf in pdf_files:

    # Grab the specific Ecoregion pdf
    # Extract ID from filename
    base = os.path.basename(pdf)

    name_part, id_part = base.split("_Id_gcs_")
    id_part = id_part.replace(".pdf", "")
    id_part = id_part.replace("Ref_", "")

    if id_part.isdigit():
        id_val = int(id_part)
        row = df.loc[df["Id_gcs"] == id_val]
    else:
        # For farms with non-numerical ID, match by farm name
        row = df.loc[df["Nombre_finca"] == name_part]

    if row.empty:
        print(f"No match found in Excel for {base}")
        continue

    # Lookup ecoregion for this farm
    Ecoregion = row["Ecoregion"].iloc[0]
    
    # Diagnostic print
    print("Checking ID:", id_val)
    print("Matches:", df.loc[df["Id_gcs"] == id_val])

    # Build correct regional pdf path
    reg_pdf = os.path.join(regional_directory, f"{Ecoregion}.pdf")

    # Create a PdfMerger object
    merger = PdfMerger()
    
    # Append the original PDF and the common PDF
    merger.append(pdf)
    merger.append(reg_pdf)
    
    # Define the output file name
    output_dir_ecoregion = os.path.join(merged_directory, Ecoregion)
    os.makedirs(output_dir_ecoregion, exist_ok=True)
    output_pdf = os.path.join(output_dir_ecoregion, f"{base}")

    
    # Output the merged PDF
    with open(output_pdf, "wb") as output_file:
        merger.write(output_file)

print("Regional PDF has been appended to each PDF successfully!")