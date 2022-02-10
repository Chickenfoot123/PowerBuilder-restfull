$PBExportHeader$uo_restful.sru
$PBExportComments$Para tratamento de WebService via objetos nativos do PB17
forward
global type uo_restful from nonvisualobject
end type
end forward

global type uo_restful from nonvisualobject
end type
global uo_restful uo_restful

type variables
Private:

string as_colunas[], ia_header_name[], ia_header_value[], ia_body_name[], ia_body_value[], ia_query_param_name[], ia_query_param_value[], ia_errors[]

string is_json, is_url, is_in_log_ativo, is_ds_dir_log, is_ds_nm_arq_log
integer ii_nr_arq_log, il_tam_arq_log
integer ii_versao_seguranca
long il_return_code

//Token
string is_token, is_token_auth, is_palavra_pre_token, is_nm_auth

datastore ids_datastore

httpclient http

transaction it_transacao

boolean ib_transacao = false
end variables

forward prototypes
public function long json_parser (string ps_json)
private subroutine json_columns_array (long pl_root, jsonparser json, string ps_nm_raiz)
private function long json_columns (jsonparser json)
private function long json_ds ()
private function long json_value_object (long pl_root, jsonparser json, string ps_nm_raiz, long pl_row)
private function long json_columns_object (long pl_root, jsonparser json, string ps_nm_raiz)
private function long json_value_array (long pl_root, jsonparser json, string ps_nm_raiz, long pl_row)
private function long json_value (jsonparser json)
public function string uf_get_value (string as_name, long pl_row)
public function string uf_get_value (string as_name_pai, string as_name, long pl_row)
public function string uf_get_value_string (string as_name, long pl_row)
public function string uf_get_value_string (string as_name_pai, string as_name, long pl_row)
public function long uf_get_value_long (string as_name_pai, string as_name, long pl_row)
public function long uf_get_value_long (string as_name, long pl_row)
public function integer uf_get_value_integer (string as_name_pai, string as_name, long pl_row)
public function integer uf_get_value_integer (string as_name, long pl_row)
public function double uf_get_value_double (string as_name_pai, string as_name, long pl_row)
public function double uf_get_value_double (string as_name, long pl_row)
public function decimal uf_get_value_decimal (string as_name_pai, string as_name, long pl_row)
public function decimal uf_get_value_decimal (string as_name, long pl_row)
public function datetime uf_get_value_datetime (string as_name_pai, string as_name, long pl_row)
public function datetime uf_get_value_datetime (string as_name, long pl_row)
public function date uf_get_value_date (string as_name_pai, string as_name, long pl_row)
public function date uf_get_value_date (string as_name, long pl_row)
public function boolean uf_get_value_boolean (string as_name_pai, string as_name, long pl_row)
public function boolean uf_get_value_boolean (string as_name, long pl_row)
public function string uf_get_json ()
public subroutine uf_set_header (string as_name, string as_value)
public subroutine uf_set_json (string as_name, string as_value)
public subroutine uf_set_query_param (string as_name, string as_value)
public function string uf_build_json ()
public function integer uf_build_token ()
private subroutine uf_build_header ()
private subroutine uf_build_path_param ()
private function string uf_build_query_param ()
public function integer uf_get_size ()
public subroutine uf_set_transaction (transaction at_transacao)
public function string uf_replaceall (string ps_string, string ps_old, string ps_new, boolean pb_ignorecase)
public function string uf_escape (string ps_value_escape)
public subroutine uf_clean_token ()
public subroutine uf_set_versao_seguranca (integer ai_versao_seguranca)
public function string uf_get_token ()
public function boolean uf_send (string ps_method, string ps_url, string ps_body)
public subroutine uf_set_error (string ps_erro)
public function boolean uf_post (string as_url, string as_body)
public function long uf_get_code ()
public subroutine uf_log (string as_method, string as_url, integer ai_code, string as_header, string as_query_param, string as_body_request, string as_body_response, double adb_runtime)
end prototypes

public function long json_parser (string ps_json);string ls_erro, ls_key
long ll_root, ll_linha, ll_count, ll_linha2, ll_root_colunas, ll_count_colunas, ll_linha3, ll_item, ll_index, ll_array
boolean lb_existe
jsonparser json

if len(ps_json) = 0 then return -1

json = create jsonparser

if mid(trim(ps_json), 1, 1) <> '[' then
	ps_json = '[' + ps_json + ']'
end if

ls_erro = json.loadstring(ps_json)

if len(trim(ls_erro)) > 0 then
	messagebox("Failed","load json failed:" + ls_erro)
	return -1
end if

json_columns(json)
json_ds()
json_value(json)

return 1
end function

private subroutine json_columns_array (long pl_root, jsonparser json, string ps_nm_raiz);long ll_loop, ll_object, ll_loop2, ll_item, ll_index, ll_array, ll_linha
string ls_key
boolean lb_existe

for ll_loop = 1 to json.getchildcount(pl_root)
	ll_object = json.getchilditem(pl_root,ll_loop)
	
	for ll_loop2 = 1 to json.getchildcount(ll_object)
		ll_item = json.getchilditem(ll_object, ll_loop2)
		ls_key = json.getchildkey(ll_object, ll_loop2)
		
		for ll_linha = 1 to upperbound(as_colunas)
			if ps_nm_raiz + '_' + ls_key = as_colunas[ll_linha] then
				lb_existe = true
			end if
		next
		
		if lb_existe = true then continue
		
		choose case json.getitemtype(ll_item)
			case jsonarrayitem!
				ll_array = json.getitemarray(ll_object, ls_key)
				json_columns_array(ll_array, json, ps_nm_raiz + '_' + ls_key)
			case jsonobjectitem!
				ll_array = json.getitemobject(ll_object, ls_key)
				json_columns_object(ll_array, json, ps_nm_raiz + '_' + ls_key)
			case else
				ll_index = upperbound(as_colunas) + 1
				as_colunas[ll_index] = ps_nm_raiz + '_' + ls_key
				
		end choose
		
	next
	
next
end subroutine

private function long json_columns (jsonparser json);long ll_root, ll_linha, ll_count, ll_linha2, ll_root_colunas, ll_count_colunas, ll_linha3, ll_item, ll_index, ll_array
boolean lb_existe
string ls_key

ll_root = json.getrootitem()//Raiz do json
ll_count = json.getchildcount(ll_root)//numero de linhas do json

for ll_linha = 1 to ll_count
	
	ll_root_colunas = json.getchilditem(ll_root, ll_linha)//Raiz das colunas
	ll_count_colunas = json.getchildcount(ll_root_colunas)//Numero de colunas do json
	
	for ll_linha2 = 1 to ll_count_colunas
		ll_item = json.getchilditem(ll_root_colunas, ll_linha2)
		ls_key = json.getchildkey(ll_root_colunas, ll_linha2)
		
		choose case json.getitemtype(ll_item)
			case jsonarrayitem!
				ll_array = json.getitemarray(ll_root_colunas, ls_key)
				json_columns_array(ll_array, json, ls_key)
			case jsonobjectitem!
				ll_array = json.getitemobject(ll_root_colunas, ls_key)
				json_columns_object(ll_array, json, ls_key)
				
			case else
		
				lb_existe = false
				
				for ll_linha3 = 1 to upperbound(as_colunas)
					
					if ls_key = as_colunas[ll_linha3] then
						lb_existe = true
					end if
					
				next
				
				if not lb_existe then
					ll_index = upperbound(as_colunas) + 1
					as_colunas[ll_index] = ls_key
				end if
		end choose
	next
			
next

return 1
end function

private function long json_ds ();string ls_syntax, error_create
long ll_linha

ls_syntax = 'release 17;' + &
'datawindow(units=0 timer_interval=0 color=1073741824 brushmode=0 transparency=0 gradient.angle=0 gradient.color=8421504 gradient.focus=0 gradient.repetition.count=0 gradient.repetition.length=100 gradient.repetition.mode=0 gradient.scale=100 gradient.spread=100 gradient.transparency=0 picture.blur=0 picture.clip.bottom=0 picture.clip.left=0 picture.clip.right=0 picture.clip.top=0 picture.mode=0 picture.scale.x=100 picture.scale.y=100 picture.transparency=0 processing=0 HTMLDW=no print.printername="" print.documentname="" print.orientation = 0 print.margin.left = 110 print.margin.right = 110 print.margin.top = 96 print.margin.bottom = 96 print.paper.source = 0 print.paper.size = 0 print.canusedefaultprinter=yes print.prompt=no print.buttons=no print.preview.buttons=no print.cliptext=no print.overrideprintjob=no print.collate=yes print.background=no print.preview.background=no print.preview.outline=yes hidegrayline=no showbackcoloronxp=no picture.file="" )' + &
'header(height=0 color="536870912" transparency="0" gradient.color="8421504" gradient.transparency="0" gradient.angle="0" brushmode="0" gradient.repetition.mode="0" gradient.repetition.count="0" gradient.repetition.length="100" gradient.focus="0" gradient.scale="100" gradient.spread="100" )' + &
'summary(height=0 color="536870912" transparency="0" gradient.color="8421504" gradient.transparency="0" gradient.angle="0" brushmode="0" gradient.repetition.mode="0" gradient.repetition.count="0" gradient.repetition.length="100" gradient.focus="0" gradient.scale="100" gradient.spread="100" )' + &
'footer(height=0 color="536870912" transparency="0" gradient.color="8421504" gradient.transparency="0" gradient.angle="0" brushmode="0" gradient.repetition.mode="0" gradient.repetition.count="0" gradient.repetition.length="100" gradient.focus="0" gradient.scale="100" gradient.spread="100" )' + &
'detail(height=0 color="536870912" transparency="0" gradient.color="8421504" gradient.transparency="0" gradient.angle="0" brushmode="0" gradient.repetition.mode="0" gradient.repetition.count="0" gradient.repetition.length="100" gradient.focus="0" gradient.scale="100" gradient.spread="100" )' + &
'table('

for ll_linha = 1 to upperbound(as_colunas)
	
	ls_syntax += 'column=(type=char(2000) updatewhereclause=yes name=' + as_colunas[ll_linha] + ' dbname="' + as_colunas[ll_linha] + '")'
	
next

ls_syntax += ' )' + &
'htmltable(border="1" )' + &
'htmlgen(clientevents="1" clientvalidation="1" clientcomputedfields="1" clientformatting="0" clientscriptable="0" generatejavascript="1" encodeselflinkargs="1" netscapelayers="0" pagingmethod=0 generatedddwframes="1" )' + &
'xhtmlgen() cssgen(sessionspecific="0" )' + &
'xmlgen(inline="0" )' + &
'xsltgen()' + &
'jsgen()' + &
'export.xml(headgroups="1" includewhitespace="0" metadatatype=0 savemetadata=0 )' + &
'import.xml()'

ids_datastore = create datastore

ids_datastore.Create(ls_syntax, error_create)

IF Len(error_create) > 0 THEN
	messagebox('', error_create)
	return -1
END IF

//ids_datastore.saveas('C:\@Trabalho\ds_json_sem_dados.xls', excel!, true)

return 1
end function

private function long json_value_object (long pl_root, jsonparser json, string ps_nm_raiz, long pl_row);long ll_loop, ll_object, ll_loop2, ll_item, ll_index, ll_array, ll_linha, ll_row_orig
string ls_key, ls_value
boolean lb_existe

ll_row_orig = pl_row

for ll_loop = 1 to json.getchildcount(pl_root)

	ll_item = json.getchilditem(pl_root, ll_loop)
	ls_key = json.getchildkey(pl_root, ll_loop)
	
//	if ll_loop > 1 /*and ids_datastore.rowcount() > 1*/ then
//		ids_datastore.rowscopy(ll_row_orig, ll_row_orig, primary!, ids_datastore, pl_row, primary!)
//	end if
	
	choose case json.getitemtype(ll_item)
		case JsonStringItem!
			ls_value = json.getitemstring(ll_item)
		case JsonNumberItem!
			ls_value = string(json.getitemnumber(ll_item))
		case JsonBooleanItem!
			ls_value = string(json.getitemboolean(ll_item))
		case JsonNullItem!
			setnull(ls_value)			
		case jsonarrayitem!
			ll_array = json.getitemarray(ll_object, ls_key)
			json_value_array(ll_array, json, ps_nm_raiz + '_' + ls_key, pl_row)
			continue
		case jsonobjectitem!
			ll_array = json.getitemobject(ll_object, ls_key)
			json_value_object(ll_array, json, ps_nm_raiz + '_' + ls_key, pl_row)
			continue
		case else
			setnull(ls_value)
	end choose
	
	ids_datastore.setitem(pl_row, ps_nm_raiz + '_' + ls_key, ls_value)
	
next

return 1
end function

private function long json_columns_object (long pl_root, jsonparser json, string ps_nm_raiz);long ll_loop, ll_object, ll_loop2, ll_item, ll_index, ll_array, ll_linha
string ls_key
boolean lb_existe

for ll_loop = 1 to json.getchildcount(pl_root)
	//ll_object = json.getchilditem(pl_root,ll_loop)

	ll_item = json.getchilditem(pl_root, ll_loop)
	ls_key = json.getchildkey(pl_root, ll_loop)
	
	for ll_linha = 1 to upperbound(as_colunas)
		if ps_nm_raiz + '_' + ls_key = as_colunas[ll_linha] then
			lb_existe = true
		end if
	next
	
	if lb_existe = true then continue
	
	choose case json.getitemtype(ll_item)
		case jsonarrayitem!
			ll_array = json.getitemarray(ll_object, ls_key)
			json_columns_array(ll_array, json, ps_nm_raiz + '_' + ls_key)
		case jsonobjectitem!
			ll_array = json.getitemobject(ll_object, ls_key)
			json_columns_object(ll_array, json, ps_nm_raiz + '_' + ls_key)
		case else
			ll_index = upperbound(as_colunas) + 1
			as_colunas[ll_index] = ps_nm_raiz + '_' + ls_key
			
	end choose
	
next

return 1
end function

private function long json_value_array (long pl_root, jsonparser json, string ps_nm_raiz, long pl_row);long ll_loop, ll_object, ll_loop2, ll_item, ll_index, ll_array, ll_linha, ll_row_orig
string ls_key, ls_value
boolean lb_existe

ll_row_orig = pl_row

for ll_loop = 1 to json.getchildcount(pl_root)
	ll_object = json.getchilditem(pl_root,ll_loop)

	if ll_loop > 1 /*and ids_datastore.rowcount() > 1*/ then
		ids_datastore.rowscopy(ll_row_orig, ll_row_orig, primary!, ids_datastore, pl_row, primary!)
	end if
	
	for ll_loop2 = 1 to json.getchildcount(ll_object)
		ll_item = json.getchilditem(ll_object, ll_loop2)
		ls_key = json.getchildkey(ll_object, ll_loop2)
		
		choose case json.getitemtype(ll_item)
			case JsonStringItem!
				ls_value = json.getitemstring(ll_item)
			case JsonNumberItem!
				ls_value = string(json.getitemnumber(ll_item))
			case JsonBooleanItem!
				ls_value = string(json.getitemboolean(ll_item))
			case JsonNullItem!
				setnull(ls_value)				
			case jsonarrayitem!
				ll_array = json.getitemarray(ll_object, ls_key)
				json_value_array(ll_array, json, ps_nm_raiz + '_' + ls_key, pl_row)
				continue
			case jsonobjectitem!
				continue
			case else
				setnull(ls_value)
		end choose
		
		ids_datastore.setitem(pl_row, ps_nm_raiz + '_' + ls_key, ls_value)
		
	next
	
next

return 1
end function

private function long json_value (jsonparser json);long ll_root, ll_linha, ll_count, ll_linha2, ll_root_colunas, ll_count_colunas, ll_linha3, ll_item, ll_index, ll_array, ll_row
boolean lb_existe
string ls_key, ls_value

ll_root = json.getrootitem()//Raiz do json
ll_count = json.getchildcount(ll_root)//numero de linhas do json

for ll_linha = 1 to ll_count
	
	ll_root_colunas = json.getchilditem(ll_root, ll_linha)//Raiz das colunas
	ll_count_colunas = json.getchildcount(ll_root_colunas)//Numero de colunas do json
	ll_row = ids_datastore.insertrow(0)
	
	for ll_linha2 = 1 to ll_count_colunas
		ll_item = json.getchilditem(ll_root_colunas, ll_linha2)
		ls_key = json.getchildkey(ll_root_colunas, ll_linha2)
		
		choose case json.getitemtype(ll_item)
			case JsonStringItem!
				ls_value = json.getitemstring(ll_item)
			case JsonNumberItem!
				ls_value = string(json.getitemnumber(ll_item))
			case JsonBooleanItem!
				ls_value = string(json.getitemboolean(ll_item))
			case JsonNullItem!
				setnull(ls_value)
			case jsonarrayitem!
				ll_array = json.getitemarray(ll_root_colunas, ls_key)
				json_value_array(ll_array, json, ls_key, ll_row)
				continue
			case jsonobjectitem!
				ll_array = json.getitemobject(ll_root_colunas, ls_key)
				json_value_object(ll_array, json, ls_key, ll_row)
				continue
			case else
				setnull(ls_value)
		end choose
		
		ids_datastore.setitem(ll_row, ls_key, ls_value)
		
	next
			
next

ids_datastore.saveas('C:\@Trabalho\ds_json_com_dados.xls', excel!, true)

return 1
end function

public function string uf_get_value (string as_name, long pl_row);return ids_datastore.getitemstring(pl_row, as_name)
end function

public function string uf_get_value (string as_name_pai, string as_name, long pl_row);//as_name_pai = 'chave do array ou object' EX: requisicao_id

return ids_datastore.getitemstring(pl_row, as_name_pai + '_' + as_name)
end function

public function string uf_get_value_string (string as_name, long pl_row);return uf_get_value(as_name, pl_row)
end function

public function string uf_get_value_string (string as_name_pai, string as_name, long pl_row);return uf_get_value(as_name_pai, as_name, pl_row)
end function

public function long uf_get_value_long (string as_name_pai, string as_name, long pl_row);return long(uf_get_value(as_name_pai,as_name,pl_row))
end function

public function long uf_get_value_long (string as_name, long pl_row);return long(uf_get_value(as_name,pl_row))
end function

public function integer uf_get_value_integer (string as_name_pai, string as_name, long pl_row);return integer(uf_get_value(as_name_pai, as_name, pl_row))
end function

public function integer uf_get_value_integer (string as_name, long pl_row);return integer(uf_get_value(as_name, pl_row))
end function

public function double uf_get_value_double (string as_name_pai, string as_name, long pl_row);return double(uf_get_value(as_name_pai, as_name, pl_row))
end function

public function double uf_get_value_double (string as_name, long pl_row);return double(uf_get_value(as_name, pl_row))
end function

public function decimal uf_get_value_decimal (string as_name_pai, string as_name, long pl_row);string ls_valor
long 	 ll_pos

ls_valor = uf_get_value(as_name_pai, as_name, pl_row)
ll_pos = pos(ls_valor, '.')
if ll_pos > 0 then ls_valor = replace(ls_valor, ll_pos , 1, ',' )

return dec(ls_valor)
end function

public function decimal uf_get_value_decimal (string as_name, long pl_row);string ls_valor
long 	 ll_pos

ls_valor = uf_get_value(as_name, pl_row)
ll_pos = pos(ls_valor, '.')
if ll_pos > 0 then ls_valor = replace(ls_valor, ll_pos , 1, ',' )

return dec(ls_valor)
end function

public function datetime uf_get_value_datetime (string as_name_pai, string as_name, long pl_row);return Datetime(uf_get_value(as_name_pai, as_name, pl_row))
end function

public function datetime uf_get_value_datetime (string as_name, long pl_row);return Datetime(uf_get_value(as_name, pl_row))
end function

public function date uf_get_value_date (string as_name_pai, string as_name, long pl_row);return Date(uf_get_value(as_name_pai, as_name, pl_row))
end function

public function date uf_get_value_date (string as_name, long pl_row);return Date(uf_get_value(as_name, pl_row))
end function

public function boolean uf_get_value_boolean (string as_name_pai, string as_name, long pl_row);if upper(trim(uf_get_value(as_name_pai, as_name, pl_row)))="TRUE" then
	return true
else
	return false
end if
end function

public function boolean uf_get_value_boolean (string as_name, long pl_row);if upper(trim(uf_get_value(as_name, pl_row)))="TRUE" then
	return true
else
	return false
end if
end function

public function string uf_get_json ();return is_json
end function

public subroutine uf_set_header (string as_name, string as_value);integer li_indice, li_update

li_update=0

for li_indice=1 to UpperBound(ia_header_name)
	if ia_header_name[li_indice] = as_name then 
		li_update=1
		ia_header_value[li_indice] = as_value
		return 
	end if
next

if li_update=0 then
	li_indice = UpperBound(ia_header_name)
	li_indice++
	
	ia_header_name[li_indice] = as_name
	ia_header_value[li_indice] =as_value
end if
end subroutine

public subroutine uf_set_json (string as_name, string as_value);integer li_indice, li_update

li_update=0

for li_indice=1 to UpperBound(ia_body_name)
	if ia_body_name[li_indice] = as_name and as_name <> '' then 
		li_update=1
		ia_body_value[li_indice] = as_value
		return 
	end if
next

if li_update=0 then
	li_indice = UpperBound(ia_body_name)
	li_indice++
	
	ia_body_name[li_indice] = as_name
	ia_body_value[li_indice] =as_value
end if
end subroutine

public subroutine uf_set_query_param (string as_name, string as_value);integer li_indice, li_update

li_update=0

for li_indice=1 to UpperBound(ia_query_param_name)
	if ia_query_param_name[li_indice] = as_name then 
		li_update=1
		ia_query_param_value[li_indice] = as_value
		return 
	end if
next

if li_update=0 then
	li_indice = UpperBound(ia_query_param_name)
	li_indice++
	
	ia_query_param_name[li_indice] = as_name
	ia_query_param_value[li_indice] =as_value
end if
end subroutine

public function string uf_build_json ();//Monta o json caso deja necessario passar como parametro em algum Endpoint
integer li_i, li_len, la_pais[], li_i_pais , li_min, li_max, fl_vector=0
string ls_body, ls_str, ls_str_ant, ls_name, ls_value,la_empty[]

for li_i=1 to  UpperBound(ia_body_name)

	ls_name = ia_body_name[li_i]
	ls_value = ia_body_value[li_i]	   
 
	if ls_name='' then
		if mid(ls_str_ant,len(ls_str_ant)-1,2) <> '},' then                                   
			ls_str +=  '['
		end if
	else
		ls_str += '"'+ls_name+'":'
	end if

	if ls_name = '' then
		ls_str += ls_value+','
	else

		if left(ls_value,1) = '{' or left(ls_value,1) = '[' then		
			if right(ls_value,1) <> '}' and right(ls_value,1) <> ']'  then
				fl_vector=1	
			end if
			ls_str += ls_value+','
		else
			ls_str += '"'+ls_value+'",';
		end if
	end if		
	if left(ls_str,1) ='[' then
		ls_str = mid(ls_str,2,len(ls_str))
	end if
	ls_str_ant = ls_str

next

if ls_name <> '' then
	ls_str = '{'+ mid(ls_str,1,(len(ls_str)-1))	
	
	if fl_vector = 1 then
		ls_str = mid(ls_str,1,(len(ls_str)-1))	+']'
	end if
	ls_str+= '}';
else
	ls_str = '['+mid(ls_str,1,(len(ls_str)-1))	+']';
end if

//limpa arrays
ia_body_name = la_empty
ia_body_value = la_empty

return ls_str
end function

public function integer uf_build_token ();/*Gera o token para acesso ao microservico*/
string ls_clientid, ls_clientsecret, ls_username, ls_password, ls_grant, ls_url_token
long ll_retorno
TokenRequest lnv_TokenRequest
TokenResponse lnv_TokenResponse
OAuthClient client

ls_grant = 'password'

SELECT client_id,
		   client_secret,
		   cd_usuario_conexao,
		   senha_conexao,
		   ds_url
INTO :ls_clientid,
	    :ls_clientsecret,
	    :ls_username,
	    :ls_password,
	    :ls_url_token
FROM microservico
WHERE id_microservico = 'Authorization'	
AND cd_versao = 1
USING it_transacao;

if it_transacao.sqlcode = -1 then
	messagebox('Atenção', 'Erro ao gerar token de autenticação. Erro: ' + it_transacao.sqlerrtext)
	return -1
end if

ls_password = uf_escape(ls_password)

lnv_TokenRequest.clientid = ls_clientid
lnv_TokenRequest.clientsecret = ls_clientsecret
lnv_TokenRequest.username = ls_username
lnv_TokenRequest.password = ls_password
lnv_TokenRequest.granttype = ls_grant
lnv_TokenRequest.method = "POST"
lnv_TokenRequest.Tokenlocation =ls_url_token

client = CREATE OAuthClient
ll_retorno = client.AccessToken ( lnv_TokenRequest, lnv_TokenResponse )
Destroy client

if ll_retorno = -1 then
	messagebox('Atenção', 'Não foi possivel gerar o token de autenticação (1).')
	return -1
end if

is_token = lnv_TokenResponse.getaccesstoken()

if trim(is_token) = '' or isnull(is_token) then
	messagebox('Atenção', 'Não foi possivel gerar o token de autenticação (2).')
	return -1
end if

return 1
end function

private subroutine uf_build_header ();string la_empty[]
long ll_linha

uf_set_header( 'Cache-Control', 'no-cache')
uf_set_header('Content-Type', 'application/json')
uf_set_header( 'If-Modified-Since', 'Thu, 1 Jan 1970 00:00:00 GMT')

for ll_linha = 1 to UpperBound(ia_header_name)
	http.setrequestheader(ia_header_name[ll_linha], ia_header_value[ll_linha])
next

//limpa arrays
ia_body_name = la_empty
ia_body_value = la_empty
end subroutine

private subroutine uf_build_path_param ();//Queryparm serve para montar a URL quando os parametros não obrigatorios são passados via HTTP (concatenar os parametros com a URL)
integer li_indice, li_len
string ls_query_param, la_empty[]

for li_indice=1 to UpperBound(ia_query_param_name)
	if ia_query_param_name[li_indice] <> ''	then
		is_url = replace(is_url, 1, len(ia_query_param_name[li_indice]) + 2, "{" + ia_query_param_value[li_indice] + "}")
	end if
next

li_len = Len(ls_query_param)-1

//limpa arrays
ia_query_param_name = la_empty
ia_query_param_value = la_empty
end subroutine

private function string uf_build_query_param ();//Queryparm serve para montar a URL quando os parametros são passados via URL
//EX: https://teste_ms.com.br/microservico?parametro=VALOR
integer li_indice, li_len
string ls_query_param, la_empty[]

for li_indice=1 to UpperBound(ia_query_param_name)
	if ia_query_param_name[li_indice] <> ''	then
		ls_query_param += ia_query_param_name[li_indice] + '=' + ia_query_param_value[li_indice] + '&'
	end if
next

li_len = Len(ls_query_param)-1

//limpa arrays
ia_query_param_name = la_empty
ia_query_param_value = la_empty

return left(ls_query_param,li_len)
end function

public function integer uf_get_size ();return ids_datastore.rowcount()
end function

public subroutine uf_set_transaction (transaction at_transacao);it_transacao = at_transacao
string ls_cd_parametro, ls_vl_parametro

if isValid(it_transacao) then
	ib_transacao = true
	
	DECLARE cursor_param_geral CURSOR FOR	
	
		SELECT cd_parametro, vl_parametro 
		  FROM parametro_geral 
		 WHERE ds_funcionalidade = 'REST'
		 USING it_transacao;
	
		OPEN cursor_param_geral;
	
		DO WHILE 0 = 0
		FETCH cursor_param_geral INTO :ls_cd_parametro, :ls_vl_parametro;
													  
			IF  it_transacao.SQLCode <> 0 THEN EXIT 
	
			ls_cd_parametro = trim(ls_cd_parametro)
			ls_vl_parametro = trim(ls_vl_parametro)
	
			IF ls_cd_parametro = 'in_log_ativo' THEN
				is_in_log_ativo = ls_vl_parametro
			ELSEIF ls_cd_parametro = 'ds_dir_log' THEN
				is_ds_dir_log = ls_vl_parametro
			ELSEIF ls_cd_parametro = 'ds_nm_arq_log' THEN
				is_ds_nm_arq_log = ls_vl_parametro			
			ELSEIF ls_cd_parametro = 'nr_arq_log' THEN
				ii_nr_arq_log = integer(ls_vl_parametro)
			ELSEIF ls_cd_parametro = 'tam_arq_log' THEN
				il_tam_arq_log = long(ls_vl_parametro)			
			END IF
																
		LOOP	 
	
	CLOSE cursor_param_geral;
	
	If not DirectoryExists ( is_ds_dir_log ) Then
		CreateDirectory ( is_ds_dir_log )
	end if	

	If right(is_ds_dir_log, 1) <> "\" then is_ds_dir_log += "\"
	 
end if
end subroutine

public function string uf_replaceall (string ps_string, string ps_old, string ps_new, boolean pb_ignorecase);////////////////////////////////////////////////////////////////////////////////
//
//    Parametros:
//    ps_string			String a ser pesquisada
//    ps_old				String antiga a ser substituida
//    ps_new				String nova para substituir
//    pb_ignorecase	Boolean pra case sensitivity
//
//    Retornos:        String
//                        ps_string com todas as ocorrencias do ps_old sibstituidos por ps_new.
//                        Se o valor de qualquer argumento for NULL, a função retornará NULL.
//
//    Descrição: Substitua todas as ocorrências de uma string dentro de outra por uma nova string.
//
// * Copyright (c) 2004-2005, All rights reserved.
// * http://www.gnu.org/copyleft/lesser.html
//////////////////////////////////////////////////////////////////////////////
 
Long    ll_Start
Long    ll_OldLen
Long    ll_NewLen
String ls_string
string ls_null
 
//Confere os parametros
If IsNull(ps_string) or IsNull(ps_old) or IsNull(ps_new) or IsNull(pb_ignorecase) Then
	SetNull(ls_null)
	Return ls_null
End If
 
//Pega o tamanho das strings
ll_OldLen = Len(ps_old)
ll_NewLen = Len(ps_new)
 
//Deve funcionar respeitando o case sensitivity
If pb_ignorecase Then
	ps_old = Lower(ps_old)
	ls_string = Lower(ps_string)
Else
    ls_string = ps_string
End If
 
//Search for the first occurrence of ps_old
ll_Start = Pos(ls_string, ps_old)
 
Do While ll_Start > 0
    // replace ps_old with ps_new
    ps_string = Replace(ps_string, ll_Start, ll_OldLen, ps_new)
    
    //Should function respect case.
    If pb_ignorecase Then 
        ls_string = Lower(ps_string)
    Else
        ls_string = ps_string
    End If
    
    // find the next occurrence of ps_old
    ll_Start = Pos(ls_string, ps_old, (ll_Start + ll_NewLen))
Loop
 
Return ps_string
end function

public function string uf_escape (string ps_value_escape);boolean lb_exit = false
long ll_pos
string ls_string
string ls_function

ls_string = uf_replaceall(ps_value_escape, '%', '%25', true)
ls_string = uf_replaceall(ls_string, space(1), '%20', true)
ls_string = uf_replaceall(ls_string, '!', '%21', true)
ls_string = uf_replaceall(ls_string, '#', '%23', true)
ls_string = uf_replaceall(ls_string, '$', '%24', true)
ls_string = uf_replaceall(ls_string, '^', '%5E', true)
ls_string = uf_replaceall(ls_string, '&', '%26', true)
ls_string = uf_replaceall(ls_string, '(', '%28', true)
ls_string = uf_replaceall(ls_string, ')', '%29', true)
ls_string = uf_replaceall(ls_string, '=', '%3D', true)
ls_string = uf_replaceall(ls_string, ':', '%3A', true)
ls_string = uf_replaceall(ls_string, ';', '%3B', true)
ls_string = uf_replaceall(ls_string, '"', '%22', true)
ls_string = uf_replaceall(ls_string, "'", '%27', true)
ls_string = uf_replaceall(ls_string, '\', '%5C', true)
ls_string = uf_replaceall(ls_string, '?', '%3F', true)
ls_string = uf_replaceall(ls_string, '<', '%3C', true)
ls_string = uf_replaceall(ls_string, '>', '%3E', true)
ls_string = uf_replaceall(ls_string, "~~", '%7E', true)
ls_string = uf_replaceall(ls_string, "]", '%5D', true)
ls_string = uf_replaceall(ls_string, "[", '%5B', true)
ls_string = uf_replaceall(ls_string, "}", '%7D', true)
ls_string = uf_replaceall(ls_string, "{", '%7B', true)
ls_string = uf_replaceall(ls_string, '`', '%60', true)

return ls_string
end function

public subroutine uf_clean_token ();is_token = ''
is_token_auth = ''
end subroutine

public subroutine uf_set_versao_seguranca (integer ai_versao_seguranca);ii_versao_seguranca = ai_versao_seguranca
end subroutine

public function string uf_get_token ();if ii_versao_seguranca = 2 then
	is_palavra_pre_token = 'bearer '
	is_nm_auth = "Authorization"
	return is_token
else
	is_palavra_pre_token = ''
	is_nm_auth = "token"
	return is_token
end if
end function

public function boolean uf_send (string ps_method, string ps_url, string ps_body);string ls_query_param, ls_header, ls_erro
long ll_retorno

if uf_get_token() = '' then
	//if uf_build_token(is_token, ps_erro) = -1 then
	if uf_build_token() = -1 then
		//uf_log(ps_method, ps_url, ii_response_code, ls_header, ls_query_param, as_body, is_response, ldb_runtime)/*UserStory: 83038 - Task: 83272 - Rodrigo.Rosa: Melhoria de log */
		return false
	end if
end if

uf_set_header(is_nm_auth, is_palavra_pre_token + is_token)

ls_query_param = uf_build_query_param()
uf_build_header()

Try
	
	ll_retorno =  http.SendRequest (ps_method, ps_url, ps_body)
	
	setnull(ls_erro)
	
	choose case ll_retorno
		case -1
			ls_erro = 'Erro generico.'
		case -2
			ls_erro = 'URL invalida.'
		case -3
			ls_erro = 'Sem conexão com a internet.'
		case -4
			ls_erro = 'Erro de timeout'
	end choose
	
	if not isnull(ls_erro) then
		messagebox('', 'Falha ao chamar serviço buscarProcessosCcs. Erro: ' + ls_erro)
		return false
	end if
	
	il_return_code = http.getresponsestatuscode()
	
	setnull(ls_erro)
	
	choose case il_return_code
		case 500
			ls_erro = 'Erro Interno.'
		case 401
			ls_erro = 'Unauthorized'
		case 400
			ls_erro = 'Dados incorretos'
	end choose
	
	if not isnull(ls_erro) then
		messagebox('', 'Falha ao chamar serviço buscarProcessosCcs. Erro: ' + 'Código ' + string(ll_retorno) + ' ' + ls_erro)
		return false
	end if
	
	http.GetResponseBody(is_json)
	
	if json_parser(is_json) = -1 then return false

Catch (RunTimeError r)
	uf_set_error(r.GetMessage())
	return false
catch (exception e)
	uf_set_error(e.GetMessage())
	return false
finally	
//	uf_log( as_method, as_url, ii_response_code, ls_header, ls_query_param, as_body, is_response, ldb_runtime)
end try

return true
end function

public subroutine uf_set_error (string ps_erro);integer li_indice

li_indice = UpperBound(ia_errors)
li_indice++

ia_errors[li_indice] = ps_erro
end subroutine

public function boolean uf_post (string as_url, string as_body);return uf_send('POST',as_url,as_body)
end function

public function long uf_get_code ();return il_return_code
end function

public subroutine uf_log (string as_method, string as_url, integer ai_code, string as_header, string as_query_param, string as_body_request, string as_body_response, double adb_runtime);long ll_tamanho
integer li_file, ll_i, ll_j
string ls_linha, ls_nm_arquivo, ls_nm_arq_proximo
boolean lb_achei = false

if is_in_log_ativo = 'S' Then
			
	ls_linha  = String(Now(), "yyyy-mm-dd hh:mm:ss.fff") + " "
	ls_linha+= as_method + " "
	ls_linha+= as_url + " "
	ls_linha+= "HTTP " + string(ai_code) + " "
	
	if as_header <> '' then
//		ls_linha+= "HEADER: " + uf_get_header_log( as_header) + " "
	end if
	
	if as_query_param <> '' then
		ls_linha+= "QUERYPARAM: " + as_query_param + " "
	end if	
	
	if as_body_request <> '' and Pos ( as_url , 'integrador-cnab-sau-uis/integrador/cnab/sau/v1/importacao/cnab' ) = 0 then
		ls_linha+= "BODYREQUEST: " + as_body_request + " "
	end if
	
	if as_body_response <> '' then
		ls_linha+= "BODYRESPONSE: " + as_body_response + " "
	end if

	ls_linha+= "RUNTIME: " + string(adb_runtime) + "ms"
			
	ls_nm_arquivo = f_replace_char(is_ds_nm_arq_log, 'xx', String(ii_nr_arq_log,'00'))
	If FileExists(is_ds_dir_log + ls_nm_arquivo) and FileLength(is_ds_dir_log + ls_nm_arquivo) < il_tam_arq_log Then
		ls_nm_arq_proximo =  f_replace_char(is_ds_nm_arq_log, 'xx', '01')
	Else					
				
		ll_i = 1		
		DO WHILE not lb_achei
			ls_nm_arquivo = f_replace_char(is_ds_nm_arq_log, 'xx', String(ll_i,'00'))
			if FileExists(is_ds_dir_log + ls_nm_arquivo) then
	
				if FileLength(is_ds_dir_log + ls_nm_arquivo) < il_tam_arq_log then
					lb_achei = true
				else
									
					if ll_i = ii_nr_arq_log then
						ls_nm_arquivo = ''
						lb_achei = true
					else 
						ll_i++
					end if
					
				end if			
				
			else
				lb_achei = true
			end if 
			
		loop
	
		ll_j = ll_i+1
		ls_nm_arq_proximo =  f_replace_char(is_ds_nm_arq_log, 'xx', string(ll_j,'00'))
		
	End If	
		
	filedelete(is_ds_dir_log + ls_nm_arq_proximo)		
		
	if ls_nm_arquivo = '' then
		ls_nm_arquivo = f_replace_char(is_ds_nm_arq_log, 'xx', '01')
	end if
	
	li_file = FileOpen(is_ds_dir_log + ls_nm_arquivo, LineMode!, Write!, Shared!, Append!,EncodingUTF8!)
	FileWriteEx(li_file, ls_linha)	
	FileClose(li_file)
	
End If
end subroutine

event constructor;//json= create JSONParser

http = create httpclient
end event

on uo_restful.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_restful.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event destructor;//destroy(json)
destroy(http)

if isvalid(ids_datastore) then destroy(ids_datastore)
end event

