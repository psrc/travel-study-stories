
import pandas as pd

codebook_file = r'J:\Projects\Surveys\HHTravel\Survey2019\Data\Dataset_2 August 2019\Documentation\PSRC_HTS_Codebook_080219_for_Elmer.xlsx'
codebook_w_missing = r'J:\Projects\Surveys\HHTravel\Survey2019\Data\Dataset_2 August 2019\Documentation\DataExplorerValues2019.csv'

values = pd.read_excel(codebook_file, sheet_name= 'Values')

unique_variables = values.variable.unique()
# initialize
values_w_missing = values

for var in unique_variables:
    missing_tuple=[(var,-9999,'Missing:TechnicalError', 'Missing'),
     (var,-9998, 'Missing:Non-response', 'Missing'),
     (var, 995, 'Missing:Skip logic', 'Missing' )]

    missing_tuple_df = pd.DataFrame(missing_tuple, columns =['variable', 'ValueOrder', 'ValueText', 'ValueGroup1'])
    values_w_missing = values_w_missing.append(missing_tuple_df)


values_w_missing.to_csv(codebook_w_missing, index=False)