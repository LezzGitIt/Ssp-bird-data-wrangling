## Execute this Python script to create & compile each LateX script & ultimately produce farm-specific bird biodiversity PDFs##
# Key inputs from this script were generated in the 'Outreach_farmers.R' script
# Next step: Join farm-specific PDFs with the regional 'Fun_factsl' PDFs using the 02_Merge_pdfs_working.py script

# To run this script..
# In Terminal, navigate to the directory where this script is stored using 'cd' 
# For example, cd /Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Outreach/Outreach_farmers (no quotes)
# Run 'python3 Python/01_create_pdfs_working.py' in terminal to execute this script

import os
import subprocess
import glob
import pandas as pd

# Define the output directory
output_dir = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Outreach/Outreach_farmers"
os.makedirs(output_dir, exist_ok=True)

# Load Excel created in the 'Outreach_farmers.R' script
data_filepath = f'{output_dir}/Excels/Farm_names_IDs_test.xlsx'
df = pd.read_excel(data_filepath)
data_list = df.values.tolist()
num_files = len(data_list) # Number of PDF files you want to create

# Create the LaTeX content specific to each farm
for i in range(num_files):
    latex_content = f"""
    \\documentclass[12pt]{{article}}
    \\usepackage[a4paper, top=1in, bottom=1in, left=1in, right=1in]{{geometry}}
    \\usepackage{{graphicx}} %package required to call \includegraphics
    \\usepackage{{longtable}} 
    \\usepackage{{xcolor}}
    \\usepackage{{fancyhdr}}
    \\usepackage{{pgffor}}

    \\pagestyle{{fancy}}
    \\fancyhf{{}} % Clear default headers and footers
    \\fancyhead[R]{{\\thepage}}  % Page number at the bottom right
    \\begin{{document}}
    \\pagecolor{{white}}
    \\begin{{center}}
    \\Huge \\textbf{{{data_list[i][1]}}}\\

    \\vspace{{1cm}}

    \\fontsize{{16pt}}{{16pt}}\\selectfont \\textbf{{En este reporte resumimos un poco de la diversidad de aves que encontramos en su finca. Muchísimas gracias por su colaboración!}}\\
    \par
    \\includegraphics[width=6cm, height = 2.5cm]{{{output_dir}/Images/Copia de Signature.png}}
    \par
    \\fontsize{{16pt}}{{16pt}}\\selectfont \\textbf{{Quiero agradecer el proyecto Ganadería Colombiana Sostenible por compartir las observaciones de aves, al Laboratorio de Ornitología de Cornell por los mapas de distribución, y a Griffin Gillespie y Audrey Hanson por su ayuda con este reporte.}}\\
    \par  
    \\vspace{{1.5cm}}
    \par   
    \includegraphics[width=16cm, height=13cm]{{{output_dir}/Images/SSP_landscape_{data_list[i][2]}.jpeg}}

    \\end{{center}}
    
    \\newpage
    \\fontsize{{16pt}}{{16pt}}\\selectfont \\textbf{{La diversidad de aves en su finca fue muy alta. Vimos varias especies que son raras o difíciles de ver en otros sitios, que indica que su finca está en buen estado ecológico. La presencia de tanta diversidad de aves puede beneficiar sus sistemas productivos, dado que las aves cumplen un papel muy importante con respecto a los servicios ecosistémicos que aportan. Por ejemplo, pueden ayudar con el ciclo de nutrientes, la polinización de cultivos, la dispersión de semillas, y a mantener las plagas a niveles bajos.
    La primera figura muestra cuales aves son las más abundantes en su finca, y las siguientes páginas muestran unos ejemplos de especies para que puedan seguir conociendo las aves a su alrededor!}}\\

    \\vspace{{1.5cm}}
    
    \\includegraphics[width=\\textwidth]{{{output_dir}/Farm_specific_inputs/Relative_Frequency_plots_test/{data_list[i][3]}.png}}
    
    \\newpage

    \\input{{{output_dir}/Farm_specific_inputs/Species_lists_test/{data_list[i][3]}.tex}}
    \\end{{document}}
    """
    
# Define the name of the LaTeX file and PDF file
    latex_file = f"{output_dir}/Farm_specific_PDFs/{data_list[i][3]}.tex"
    pdf_file = f"{output_dir}/Farm_specific_PDFs/{data_list[i][3]}.pdf"
    
    
# Write the LaTeX content to a .tex file
    with open(latex_file, 'w') as f:
        f.write(latex_content)
    
# Compile the .tex file into a .pdf
    subprocess.run(["pdflatex", "-output-directory", f"{output_dir}/Farm_specific_PDFs", latex_file], check=True)
    
# Delete intermediate files that end in 'aux', 'log', or 'tex'
    for ext in ['aux', 'log', 'tex']:
        file_path = f"{output_dir}/Farm_specific_PDFs/{data_list[i][3]}.{ext}"
        if os.path.exists(file_path):
            os.remove(file_path)
