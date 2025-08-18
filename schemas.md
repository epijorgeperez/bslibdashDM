Okay, I can definitely help you with that! Based on the output you provided, here's a simple markdown documentation of the tables and their schemas.

### Table Schemas Documentation

#### `DAS_DM.dbo.tb_egreso_dm`

| Column Name       | Data Type |
| :---------------- | :-------- |
| Parametro         | nvarchar  |
| Desc_Parametro    | nvarchar  |
| Fuente            | nvarchar  |
| Anio              | int       |
| Mes               | tinyint   |
| Cve_OOAD          | nchar     |
| Nombre_OOAD       | nvarchar  |
| Cve_Presupuestal  | nvarchar  |
| Nombre_Unidad     | nvarchar  |
| Cve_Especialidad  | nvarchar  |
| Especialidad      | nvarchar  |
| Sexo              | tinyint   |
| Grupo_edad        | nvarchar  |
| Dato              | float     |

#### `dbo.tb_poblacion`

| Column Name      | Data Type |
| :--------------- | :-------- |
| Parametro        | varchar   |
| Fuente           | varchar   |
| Anio             | int       |
| Mes              | int       |
| Cve_OOAD         | varchar   |
| Nombre_OOAD      | varchar   |
| Cve_Presupuestal | varchar   |
| Nombre_Unidad    | varchar   |
| Sexo             | int       |
| Grupo_edad       | varchar   |
| Poblacion        | int       |

#### `dbo.tb_censo_DM`

| Column Name      | Data Type |
| :--------------- | :-------- |
| Parametro        | varchar   |
| Fuente           | varchar   |
| Anio             | int       |
| Mes              | int       |
| Cve_OOAD         | varchar   |
| Nombre_OOAD      | varchar   |
| Cve_Presupuestal | varchar   |
| Nombre_Unidad    | varchar   |
| Sexo             | int       |
| Grupo_edad       | varchar   |
| Pacientes_DM     | int       |
| Atendidos_DM     | int       |
| PAMF             | int       |
| Prevalencia_DM   | float     |

#### `dbo.tb_consulta_dm`

| Column Name      | Data Type |
| :--------------- | :-------- |
| Parametro        | nvarchar  |
| Desc_Parametro   | nvarchar  |
| Fuente           | nvarchar  |
| Anio             | int       |
| Mes              | tinyint   |
| Cve_OOAD         | nchar     |
| Nombre_OOAD      | nvarchar  |
| Cve_Presupuestal | nvarchar  |
| Nombre_Unidad    | nvarchar  |
| Sexo             | tinyint   |
| Grupo_edad       | nvarchar  |
| Dato             | int       |


#### `dbo.tb_dm_incap`

| Column Name | Data Type |
| :---------- | :-------- |
| PERIODO     | varchar   |
| SEMEPI      | varchar   |
| NIVEL       | varchar   |
| descnivel   | varchar   |
| RAMO        | varchar   |
| descramo    | varchar   |
| LISTDX      | varchar   |
| CVEDX       | varchar   |
| descdx      | varchar   |
| TIP_SEXO    | varchar   |
| descsexo    | varchar   |
| GEDAD       | varchar   |
| descgedad   | varchar   |
| DURACI      | varchar   |
| NDIAS       | varchar   |
| FREC        | varchar   |
| TRABAJADOR  | varchar   |

#### `dbo.MORBI_DIABETES`

| Column Name      | Data Type |
| :--------------- | :-------- |
| Parametro        | nvarchar  |
| Desc_Parametro   | nvarchar  |
| Fuente           | nvarchar  |
| Anio             | int       |
| Mes              | int       |
| Cve_OOAD         | nchar     |
| Nombre_OOAD      | nvarchar  |
| Cve_Presupuestal | nvarchar  |
| Nombre_Unidad    | nvarchar  |
| Sexo             | tinyint   |
| Grupo_edad       | nvarchar  |
| Dato             | int       |

#### `dbo.MORTA_DIABETES`

| Column Name      | Data Type |
| :--------------- | :-------- |
| Parametro        | nvarchar  |
| Desc_Parametro   | nvarchar  |
| Fuente           | nvarchar  |
| Anio             | int       |
| Mes              | int       |
| Cve_OOAD         | nchar     |
| Nombre_OOAD      | nvarchar  |
| Cve_Presupuestal | nvarchar  |
| Nombre_Unidad    | nvarchar  |
| Sexo             | tinyint   |
| Grupo_edad       | nvarchar  |
| Dato             | int       |

Let me know if you need any more details or further documentation!