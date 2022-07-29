import pandas as pd


def compilador_datos(data:str) -> pd.DataFrame:
  ''' data: path del archivo csv a transformar'''

  # leemos el archivo con los datos
  df = pd.read_csv(data, encoding='latin1',on_bad_lines='skip',sep=';')

  # Modificamos los nombres de las columnas
  df.columns = ['country','code'] + [str(año) for año in range(1990,2021)]

  # transformamos los valores numericos que estan en string
  for col in [str(año) for año in range(1990,2021)]:
    df[col] = df[col].str.replace(',','.')
    df[col] = pd.to_numeric(df[col],errors='coerce')

  df = df.dropna()

  return df


if __name__ == '__main__':
    df_pib = compilador_datos('https://raw.githubusercontent.com/BautistaDavid/Twitter_Posts/main/Gif_Scatterplot/per_capita.csv')
    df_pib.to_csv('df_pib.csv')

