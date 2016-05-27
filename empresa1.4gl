SCHEMA operativa

GLOBALS
  DEFINE gr_top_tipoempresa,
         gr_top_tipoempresaaux RECORD LIKE top_tipoempresa.*

  DEFINE g_usuario  LIKE top_usuarios.usr_userid,
         g_programa LIKE top_programas.opc_rout,
         g_perfil   LIKE top_perfiles.per_codperfil,
         g_sucursal LIKE top_sucursal.suc_cod,
         g_ciudad   LIKE top_ciudad.ciu_idciudad,
         g_nom_suc  LIKE top_sucursal.suc_descripcion,
         g_accion   LIKE top_opcxprog.oxp_name         
  DEFINE gr_top_empresa
    RECORD 
      emp_idempresa    LIKE top_empresa.emp_idempresa,  
      suc_cod          LIKE top_empresa.suc_cod, 
      nomsucursal      LIKE top_sucursal.suc_descripcion,
      emp_nitempresa   LIKE top_empresa.emp_nitempresa,  
      emp_nomempresa   LIKE top_empresa.emp_nomempresa,
      emp_direccion    LIKE top_empresa.emp_direccion,
      emp_telefono     LIKE top_empresa.emp_telefono,
      emp_contacto     LIKE top_empresa.emp_contacto,
      emp_emailcontac  LIKE top_empresa.emp_emailcontac,
      emp_grupo        LIKE top_empresa.emp_grupo, 
      tip_idtipoem     LIKE top_empresa.tip_idtipoem  
    END RECORD   

  DEFINE gr_det 
    RECORD 
      emp_idempresa    LIKE top_empresa.emp_idempresa,  
      suc_cod          LIKE top_empresa.suc_cod,
      nomsucursal      LIKE top_sucursal.suc_descripcion,  
      emp_nitempresa   LIKE top_empresa.emp_nitempresa,  
      emp_nomempresa   LIKE top_empresa.emp_nomempresa,
      emp_direccion    LIKE top_empresa.emp_direccion,
      emp_telefono     LIKE top_empresa.emp_telefono,
      emp_contacto     LIKE top_empresa.emp_contacto,
      emp_emailcontac  LIKE top_empresa.emp_emailcontac,
      emp_grupo        LIKE top_empresa.emp_grupo, 
      tip_idtipoem     LIKE top_empresa.tip_idtipoem                 
    END RECORD
 
  DEFINE gar_det, gar_detaux 
    DYNAMIC ARRAY OF RECORD 
      emp_idempresa    LIKE top_empresa.emp_idempresa, 
      suc_cod          LIKE top_empresa.suc_cod,
      nomsucursal      LIKE top_sucursal.suc_descripcion, 
      emp_nitempresa   LIKE top_empresa.emp_nitempresa,  
      emp_nomempresa   LIKE top_empresa.emp_nomempresa,
      emp_direccion    LIKE top_empresa.emp_direccion,
      emp_telefono     LIKE top_empresa.emp_telefono,
      emp_contacto     LIKE top_empresa.emp_contacto,
      emp_emailcontac  LIKE top_empresa.emp_emailcontac,
      emp_grupo        LIKE top_empresa.emp_grupo,
      tip_idtipoem     LIKE top_empresa.tip_idtipoem                                         
    END RECORD

  DEFINE
      gs_opcion CHAR (1),
      gs_prm CHAR(3),
      gs_query CHAR(800),
      gs_titulo,
      gs_dessuc,
      gs_condicion CHAR(200),
      gs_archivo CHAR(30),
      gn_count,
      gn_count_enc INTEGER,
      gn_max,
      gn_max_scr SMALLINT,
      gs_msn,
      gs_log,
      gs_vl  STRING,
      gs_existe_dato,gs_act BOOLEAN,
      ln_posactual INTEGER,
      ln SMALLINT, 
      g_company LIKE top_company.cmp_ident,
      gr_sucursal RECORD LIKE top_sucursal.*,
      gr_empresa RECORD LIKE top_empresa.*
END GLOBALS 

MAIN 
  DEFINE cuantos INTEGER 
  DEFINE nombre STRING
  DEFINE gw_ppal ui.Window
  
  DEFER INTERRUPT 
  
  CLOSE WINDOW SCREEN 
 
  CONNECT TO "operativa"  
  OPTIONS INPUT WRAP 

IF arg_val(1)="M" THEN
  ##EL PROGRAMA ES EJECUTADO COMO CHILD DESDE EL TOPMENU
  CALL ui.Interface.setName("empresa")
  CALL ui.Interface.setType("child")
  CALL ui.Interface.setContainer("parent1")
  CALL ui.Interface.setText("Usuarios")
  CALL ui.Interface.refresh()
  DISPLAY ui.Interface.getChildInstances("empresa")
  LET nombre=ui.Interface.getName()
  DISPLAY nombre
END IF  
# ##CAMBIAR EMPRESAS
{#VALIDANDO INFORMACION DE USUARIO Y PERFIL
CALL fgl_getenv("LOGNAME") RETURNING g_usuario
LET g_programa="empresa"
SELECT per_codperfil INTO g_perfil FROM top_usuarios 
WHERE usr_userid=g_usuario

SELECT * FROM top_perfiles WHERE per_codperfil=g_perfil 
IF STATUS=NOTFOUND THEN
  CALL fgl_winmessage("error","Perfil de Usuario No Existe!!","error")
  EXIT PROGRAM 
ELSE 
  SELECT FIRST 1 * FROM top_progxperfil WHERE per_codperfil=g_perfil
  AND opc_codigo IN (SELECT opc_codigo FROM top_programas WHERE opc_rout=g_programa)  
  IF STATUS=NOTFOUND THEN
    CALL fgl_winmessage("error","Perfil de Usuario No tiene acceso a este Programa!!","error")
    EXIT PROGRAM
  ELSE
    #TRAE PERMISOS DE OPCION DEL PROGRAMA ACTUAL DE ACUERDO AL PERFIL DE USUARIO
    DECLARE c_per CURSOR FOR
    SELECT b.oxp_name FROM top_progxperfil a,top_opcxprog b,top_programas c
    WHERE a.per_codperfil=g_perfil AND c.opc_rout=g_programa
    AND a.oxp_codigo=b.oxp_codigo and b.opc_codigo=c.opc_codigo
  END IF
END IF  

DISPLAY "sucursal "||arg_val(2)
##SI NO ESTA EN MODO CHILD PIDE POR UNA SUCURSAL DE TRABAJO
IF arg_val(2) IS NULL THEN
  CALL choose_sucursal(g_usuario) RETURNING g_sucursal,g_nom_suc
  SELECT ciu_idciudad INTO g_ciudad
  FROM top_sucursal
  WHERE suc_cod = g_sucursal
ELSE 
  LET g_sucursal=arg_val(2)  
  SELECT ciu_idciudad INTO g_ciudad
  FROM top_sucursal
  WHERE suc_cod = g_sucursal
END IF

IF ui.Interface.getChildInstances("empresa") > 0 THEN 
  CALL fgl_winmessage("Error","Ya existe una instancia de este programa!!","error")
  EXIT PROGRAM
ELSE 
  LET cuantos=ui.Interface.getChildInstances(ui.Interface.getName())
  DISPLAY "memo"||cuantos 
END IF  } -- ojo octubre 27 de 2014

--CALL ui.Interface.loadActionDefaults("actions.4ad") ojo octubre 27 de 2014
--CALL ui.Interface.loadToolBar("toolbar.4tb") ojo octubre 27 de 2014
--CALL ui.Interface.setSize("600px","1000px") ojo octubre 27 de 2014
  
  {INITIALIZE gr_top_tipoempresa.*,gr_top_empresa.* TO NULL} --ojo octubre 27 de 2014

  --OPEN WINDOW w_mar_lin AT 2,2 WITH FORM "f_tipoempresa" ojo octubre 27 de 2014
  {OPEN WINDOW w_empresa WITH FORM "f_tipoempresa"
  LET windows = ui.Window.getCurrent()
  LET forma = windows.getform()}
  
  --LET gw_ppal = ui.Window.getCurrent()   
  --CALL gw_ppal.setText("TIPO DE EMPRESA Y EMPRESA")
  --CALL fgl_settitle("TIPO DE EMPRESA Y EMPRESA") 

  {SELECT suc_descripcion INTO gs_dessuc FROM top_sucursal 
   WHERE ciu_idciudad = g_ciudad}--octubre 21 de 2014 JHGC

  {LET gs_titulo = "TIPO DE EMPRESA Y EMPRESA - ", gs_dessuc
  CALL fgl_settitle(gs_titulo)}

  --**** NUEVO
  CALL fgl_getenv("LOGNAME") RETURNING g_usuario
    IF arg_val(2) IS NULL THEN
      CALL choose_sucursal2(g_usuario) RETURNING g_sucursal,g_nom_suc,g_company
    ELSE 
     LET g_sucursal=arg_val(2)  
END IF


### REVISANDO QUE EL USUARIO TIENE PERMISOS EN LA SUCURSAL

SELECT * FROM top_usersucu WHERE usr_userid=g_usuario 
AND suc_cod=g_sucursal

IF STATUS=NOTFOUND THEN
  CALL fgl_winmessage("Revise","No tiene Permisos en Esta Sucursal!!","Information")
  EXIT PROGRAM 
ELSE 
  ##  
END IF


SELECT * INTO gr_sucursal.* FROM top_sucursal 
WHERE suc_cod=g_sucursal

CALL ui.Interface.loadActionDefaults("actions.4ad")
CALL ui.interface.loadToolBar("toolbar.4tb")

###PREGUNTAR COMO ES LA ASIGNACION DE LA EMPRESA
SELECT * INTO gr_empresa.* FROM top_empresa 
WHERE suc_cod=2 AND emp_idempresa=1

  OPEN WINDOW w_mar_lin AT 2,2 WITH FORM "f_tipoempresa"
  LET gw_ppal = ui.Window.getCurrent()  
   
  CALL gw_ppal.setText("AUTORIZACIONES")
  
  --**** FIN DE LO NUEVO

  MENU --"" ATTRIBUTES (STYLE="Window.main2",COMMENT="Menu de Tipo de Empresa",IMAGE="attention")
   {BEFORE MENU 
     WHENEVER ERROR CONTINUE
      CALL DIALOG.setActionActive("new",FALSE)
      CALL DIALOG.setActionActive("query",FALSE)
    FOREACH c_per INTO g_accion 
      CALL DIALOG.setActionActive(g_accion CLIPPED,TRUE)
    END FOREACH
    WHENEVER ERROR STOP } --OCTUBRE 27 DE 2014
    ON ACTION NEW 
      LET gs_opcion = "A" 
      CALL inicializar_todo()
      IF input_tipoempresa() THEN 
      END IF
           
    ON ACTION query
      LET gs_opcion = "C"
      CALL inicializar_todo()      
      IF construct_datos() THEN
        CALL recorre_consulta()
      END IF    
   
    ON ACTION EXIT
      EXIT MENU   
  END MENU 
END MAIN 

FUNCTION inicializar_todo()

  DEFINE ls_titulo VARCHAR (200)
  
  IF gs_opcion != "M" AND  gs_opcion != "I" THEN 
    INITIALIZE gr_top_tipoempresa.*, gr_top_empresa.* TO NULL 
    INITIALIZE gs_query,gs_condicion,gs_archivo TO NULL
    CALL gar_det.clear()
  END IF 
   
  LET gn_count = 0
  LET gn_max = 100
  LET gn_max_scr = 10

  IF gs_opcion = "C"  THEN
    CLEAR FORM
  END IF
  
  LET ls_titulo = "Tipo de Empresa y Empresas", gs_dessuc
  
  IF gs_opcion = "A" THEN 
    LET ls_titulo = "Adicionar  ", ls_titulo CLIPPED   
  END IF
  
  IF gs_opcion = "C" THEN 
    LET ls_titulo = "Consultar  ", ls_titulo CLIPPED
  END IF

  CALL fgl_settitle(ls_titulo)  
END FUNCTION 

##TIPO EMPRESA
FUNCTION input_tipoempresa()

  DEFINE ln_ok, 
         ln_idx,
         ln_scrn,
         ln_i, 
         ln_max,
         ln_ind SMALLINT

  DEFINE x INTEGER 
         
  LET ln_ok = TRUE 
  LET ln_idx = 1
  LET ln_scrn = 1

  IF gs_opcion = "A" THEN
    INITIALIZE gr_top_tipoempresa.*, gr_top_empresa.* TO NULL  
  ELSE  
    INITIALIZE gr_top_tipoempresaaux.* TO NULL 
    LET gr_top_tipoempresaaux.* = gr_top_tipoempresa.*    
    CALL gar_detaux.clear()
    CALL gar_det.getLength() RETURNING ln_max

    FOR ln_i  = 1 TO ln_max
      IF gar_det[ln_i].emp_idempresa > 0 THEN
        CALL gar_detaux.appendElement()
        LET gar_detaux[ln_i].* = gar_det[ln_i].*
      END IF 
    END FOR 
  END IF

  DIALOG ATTRIBUTES(UNBUFFERED)

    INPUT BY NAME gr_top_tipoempresa.tip_idtipoem,gr_top_tipoempresa.tip_nomtipoem ATTRIBUTES (WITHOUT DEFAULTS)
      BEFORE INPUT  
      DISPLAY BY NAME gr_top_tipoempresa.tip_nomtipoem

      IF gs_opcion != "M" THEN 
        DISPLAY gr_top_tipoempresa.tip_idtipoem TO tip_idtipoem --jhgc 
      END IF 
      
      AFTER FIELD tip_nomtipoem

        IF (gr_top_tipoempresa.tip_nomtipoem IS NULL OR gr_top_tipoempresa.tip_nomtipoem = " ") THEN 
          CALL fgl_winmessage("Informacion","Debe digitar el tipo de empresa","information")
          NEXT FIELD tip_nomtipoem 
        END IF
   
        LET gn_count = 0
      
        SELECT count(*) INTO gn_count FROM top_tipoempresa
         WHERE tip_idtipoem != gr_top_tipoempresa.tip_idtipoem         
    
        IF gn_count IS NULL  THEN
          LET gn_count = 0 
        END IF

        IF (gn_count > 0) THEN
        END IF 

        ##VALIDA SI EXISTE DATO EN TIPO EMPRESA
        --**
        IF gs_opcion != "M" THEN
          IF (gr_top_tipoempresa.tip_nomtipoem IS NOT NULL) THEN
            CALL existe_dato("top_tipoempresa","tip_nomtipoem",gr_top_tipoempresa.tip_nomtipoem) RETURNING gs_existe_dato
            IF gs_existe_dato THEN
              LET gs_msn = "El tipo de empresa ",gr_top_tipoempresa.tip_nomtipoem CLIPPED, " ya existe"
              CALL fgl_winmessage("Informacion",gs_msn,"information")
              NEXT FIELD tip_nomtipoem 
            END IF 
          END IF 
        END IF   
        --**         
    END INPUT 
    
    ## CARGA EL DETALLE DE LA GRILLA    
    INPUT ARRAY gar_det FROM scr_empresa.* ATTRIBUTES(WITHOUT DEFAULTS,INSERT ROW=FALSE)
     
       BEFORE ROW
       LET ln_scrn = scr_line()
       IF ln_idx <= gn_max_scr THEN
         DISPLAY gr_top_empresa.emp_idempresa TO idempresa --jhgc     
        --DISPLAY gar_det[ln_i].* TO scr_linea[ln_scrn].* --ln_idx     
       END IF
         CALL gar_det.getLength() RETURNING gn_max --JHGC 05032014
       AFTER ROW    
         CALL gar_det.getLength() RETURNING gn_max 
     ##JHGC 31032014
      BEFORE FIELD nomsucursal           
      LET ln_ind = arr_curr()
      LET gar_det[ln_ind].suc_cod = g_sucursal
      IF (gar_det[ln_ind].suc_cod IS NULL OR gar_det[ln_ind].emp_nitempresa = " ") THEN 
        CALL fgl_winmessage("Informacion","Debe Digitar el Codigo Sucursal","information")
        NEXT FIELD suc_cod          
      ELSE               
        SELECT top_sucursal.suc_descripcion INTO gr_top_empresa.nomsucursal
          FROM top_sucursal
         WHERE suc_cod = gar_det[ln_ind].suc_cod   
            LET gar_det[ln_ind].nomsucursal = gr_top_empresa.nomsucursal
      END IF  --**
      
      AFTER FIELD empnitempresa 
      IF gs_opcion != "M" THEN
        LET ln_ind = arr_curr()
        IF (gar_det[ln_ind].emp_nitempresa IS NULL OR gar_det[ln_ind].emp_nitempresa = " ") THEN   
          CALL fgl_winmessage("Informacion","Dede Digitar el NIT de la Empresa","information")
          NEXT FIELD empnitempresa
        END IF
      
        IF (gar_det[ln_ind].emp_nitempresa IS NOT NULL) THEN 
          LET gs_prm = "nit"       
          CALL existe_dato_("top_empresa","emp_nitempresa","suc_cod",gar_det[ln_ind].emp_nitempresa,g_sucursal)RETURNING gs_existe_dato
        
        SELECT ts.suc_descripcion INTO gr_top_empresa.nomsucursal
          FROM top_sucursal ts, top_empresa tm
         WHERE  ts.suc_cod = tm.suc_cod
           AND tm.emp_nitempresa = gar_det[ln_ind].emp_nitempresa

          IF gs_existe_dato THEN
            LET gs_msn = "El NIT ",gar_det[ln_ind].emp_nitempresa  CLIPPED , " ya exite en esta sucursal "--, gr_top_empresa.nomsucursal
            CALL fgl_winmessage("Informacion",gs_msn,"information")
            NEXT FIELD empnitempresa                  
          END IF        
        END IF 
      END IF 
      AFTER FIELD empnomempresa
      IF gs_opcion != "M" THEN
        LET ln_ind = arr_curr()    
        IF (gar_det[ln_ind].emp_nomempresa IS NULL OR gar_det[ln_ind].emp_nomempresa = ' ') THEN
          CALL fgl_winmessage("Informacion","Dede Digitar el Nombre de la Empresa","informaton")
          NEXT FIELD empnomempresa
        END IF

        IF (gar_det[ln_ind].emp_nomempresa IS NOT NULL) THEN 
          LET gs_prm = "nom"
          CALL existe_dato_ ("top_empresa","emp_nomempresa","suc_cod",gar_det[ln_ind].emp_nomempresa,g_sucursal)RETURNING gs_existe_dato
        
        SELECT ts.suc_descripcion INTO gr_top_empresa.nomsucursal
          FROM top_sucursal ts, top_empresa tm
         WHERE ts.suc_cod = tm.suc_cod
           AND tm.emp_nomempresa = gar_det[ln_ind].emp_nomempresa
        
          IF gs_existe_dato THEN
            LET gs_msn = "El Nombre de la Empresa ",gar_det[ln_ind].emp_nomempresa  CLIPPED , " ya existe en esta sucursal "--, gr_top_empresa.nomsucursal
            CALL fgl_winmessage("Informacion",gs_msn,"information")
            NEXT FIELD empnomempresa                  
          END IF        
        END IF 
      END IF 
      
      AFTER FIELD empdireccion
      IF gs_opcion != "M" THEN
        LET ln_ind = arr_curr()
        IF (gar_det[ln_ind].emp_direccion IS NULL OR gar_det[ln_ind].emp_direccion = " ") THEN
          CALL fgl_winmessage("Informacion","Dede Digitar la Direccion de la Empresa","information")
          NEXT FIELD empdireccion
        END IF 
      END IF 
      
      AFTER FIELD emptelefono
      IF gs_opcion != "M" THEN
        LET ln_ind = arr_curr()
        IF (gar_det[ln_ind].emp_telefono IS NULL OR gar_det[ln_ind].emp_telefono = " ") THEN
          CALL fgl_winmessage("Informacion","Debe Digitar el Telefono de la Empresa","information")
          NEXT FIELD emptelefono
        END IF 
      END IF 
      
      AFTER FIELD empgrupo
      IF gs_opcion != "M" THEN
        LET ln_ind = arr_curr()
        IF (gar_det[ln_ind].emp_grupo IS NULL OR gar_det[ln_ind].emp_grupo = " ") THEN
          CALL fgl_winmessage("Informacion","pertence al Grupo","information")
          NEXT FIELD empgrupo
        END IF  
      END IF   
     ##
     END INPUT
     
  ON ACTION SAVE
    CASE 
      WHEN gs_opcion = "A" 
        ##VALIDA SI EXISTE DATO EN TIPO EMPRESA
        --**
        IF (gr_top_tipoempresa.tip_nomtipoem IS NOT NULL) THEN
          CALL existe_dato("top_tipoempresa","tip_nomtipoem",gr_top_tipoempresa.tip_nomtipoem) RETURNING gs_existe_dato
          IF gs_existe_dato THEN
            LET gs_msn = "El tipo de empresa ",gr_top_tipoempresa.tip_nomtipoem CLIPPED, " ya existe"
            CALL fgl_winmessage("Informacion",gs_msn,"information")
            NEXT FIELD tip_nomtipoem           
          END IF          
        END IF
        
        IF ln_ind = 0 THEN 
          LET gs_msn = "Debe digitar la empresa correspondiente al tipo de Empresa"
          CALL fgl_winmessage("Informacion",gs_msn,"information")
          NEXT FIELD nomsucursal
        END IF
        
        IF (gar_det[ln_ind].emp_nomempresa IS NOT NULL) THEN 
          LET gs_prm = "nom"
          CALL existe_dato_ ("top_empresa","emp_nomempresa","suc_cod",gar_det[ln_ind].emp_nomempresa,g_sucursal)RETURNING gs_existe_dato
        
        SELECT ts.suc_descripcion INTO gr_top_empresa.nomsucursal
          FROM top_sucursal ts, top_empresa tm
         WHERE ts.suc_cod = tm.suc_cod
           AND tm.emp_nomempresa = gar_det[ln_ind].emp_nomempresa
        
          IF gs_existe_dato THEN
            LET gs_msn = "El Nombre de la Empresa ",gar_det[ln_ind].emp_nomempresa  CLIPPED , " ya existe en esta sucursal "--, gr_top_empresa.nomsucursal
            CALL fgl_winmessage("Informacion",gs_msn,"information")
            NEXT FIELD empnomempresa                  
          END IF        
        END IF 
        
        IF insertar_tipoempresa() THEN
          CALL fgl_winmessage("Informacion","La informacion fue actualizada","information")
        ELSE
          CALL fgl_winmessage("Stop","La informacion no fue generada","stop")
        END IF  
        DISPLAY gr_top_empresa.emp_idempresa TO idempresa
             
        --**
      WHEN gs_opcion = "M"
        IF actualiza_tipoempresa() THEN
        ELSE
        END IF  
    END CASE  
       
  EXIT DIALOG 

  ON ACTION  CANCEL 
    CALL inicializar_todo()
    LET INT_FLAG = TRUE 
    EXIT DIALOG  
      
  {ON ACTION ACCEPT}  
  {ON ACTION EXIT 
    EXIT DIALOG }
  END DIALOG 
  
  CALL gar_det.getLength() RETURNING gn_max
  IF INT_FLAG THEN
    IF gs_opcion = "A" THEN
      CALL fgl_winmessage("Informacion","El Usuario Cancela El Proceso","information")
    END IF 
    IF  gs_opcion = "M" THEN
      --CALL fgl_winmessage("Informacion","El Usuario Cancela La Modificación del Registro","information")
    END IF 
    LET INT_FLAG = FALSE 
    LET ln_ok = FALSE
  END IF 

  RETURN ln_ok
END FUNCTION

#INSERTAR EMPRESA
FUNCTION input_empresa()
  
  DEFINE
    ln_ok,
    ln_idx,
    ln_scrn,
    ln_i,
    ln_max,
    ln_ind SMALLINT,
    ls_valor STRING
  
  LET ln_scrn = scr_line()
  IF ln_idx <= gn_max_scr THEN
    DISPLAY gr_top_empresa.emp_idempresa TO idempresa --jhgc      
    --DISPLAY gar_det[ln_i].* TO scr_empresa[ln_scrn].*   --ln_idx
  END IF
  
  CALL gar_detaux.clear()
  CALL gar_det.getLength() RETURNING ln_max  
  
  FOR ln_i  = 1 TO ln_max
    IF gar_det[ln_i].emp_idempresa > 0 THEN
      CALL gar_detaux.appendElement()
      LET gar_detaux[ln_i].* = gar_det[ln_i].*
    END IF 
  END FOR

  DIALOG ATTRIBUTES(UNBUFFERED)
  
    INPUT ARRAY gar_det FROM scr_empresa.* ATTRIBUTES(WITHOUT DEFAULTS,INSERT ROW=FALSE)
    BEFORE INPUT    
     {ON ACTION zoom
      CASE        
        WHEN INFIELD(suc_cod) 
        LET ln_ind = arr_curr()
        CALL make_help_emp("top_sucursal","suc_cod", "suc_descripcion", "SUCURSALES", null) RETURNING gr_top_empresa.suc_cod
        CLOSE WINDOW w_help

        SELECT top_sucursal.suc_descripcion INTO gr_top_empresa.nomsucursal
          FROM top_sucursal
         WHERE suc_cod = gr_top_empresa.suc_cod 
         
        LET gar_det[ln_ind].suc_cod = gr_top_empresa.suc_cod
        LET gar_det[ln_ind].nomsucursal = gr_top_empresa.nomsucursal  
      END CASE}        
    BEFORE FIELD nomsucursal  
    
      LET ln_ind = arr_curr()
      LET gar_det[ln_ind].suc_cod = g_sucursal
      IF (gar_det[ln_ind].suc_cod IS NULL OR gar_det[ln_ind].emp_nitempresa = " ") THEN 
        CALL fgl_winmessage("Informacion","Debe Digitar el Codigo Sucursal","information")
        NEXT FIELD suc_cod          
      ELSE               
        SELECT top_sucursal.suc_descripcion INTO gr_top_empresa.nomsucursal
          FROM top_sucursal
         WHERE suc_cod = gar_det[ln_ind].suc_cod   
            LET gar_det[ln_ind].nomsucursal = gr_top_empresa.nomsucursal
      END IF  --**
       
    AFTER FIELD empnitempresa 
      
      LET ln_ind = arr_curr()
      IF (gar_det[ln_ind].emp_nitempresa IS NULL OR gar_det[ln_ind].emp_nitempresa = " ") THEN   
        CALL fgl_winmessage("Informacion","Dede Digitar el NIT de la Empresa","information")
        NEXT FIELD empnitempresa
      END IF
      
      IF (gar_det[ln_ind].emp_nitempresa IS NOT NULL) THEN         
        CALL existe_dato ("top_empresa","emp_nitempresa",gar_det[ln_ind].emp_nitempresa)RETURNING gs_existe_dato
        
        {SELECT ts.suc_descripcion INTO gr_top_empresa.nomsucursal
          FROM top_sucursal ts, top_empresa tm
         WHERE  ts.suc_cod = tm.suc_cod
           AND tm.emp_nitempresa = gar_det[ln_ind].emp_nitempresa}

        IF gs_existe_dato THEN
          LET gs_msn = "El NIT ",gar_det[ln_ind].emp_nitempresa  CLIPPED , " ya existe en esta sucursal"--, gr_top_empresa.nomsucursal
          CALL fgl_winmessage("Informacion",gs_msn,"information")
          NEXT FIELD empnitempresa                  
        END IF        
      END IF 
      
    AFTER FIELD empnomempresa
      LET ln_ind = arr_curr()    
      IF (gar_det[ln_ind].emp_nomempresa IS NULL OR gar_det[ln_ind].emp_nomempresa = ' ') THEN
        CALL fgl_winmessage("Informacion","Dede Digitar el Nombre de la Empresa","informaton")
        NEXT FIELD empnomempresa
      END IF

      IF (gar_det[ln_ind].emp_nomempresa IS NOT NULL) THEN         
        CALL existe_dato ("top_empresa","emp_nomempresa",gar_det[ln_ind].emp_nomempresa)RETURNING gs_existe_dato
        
        {SELECT ts.suc_descripcion INTO gr_top_empresa.nomsucursal
          FROM top_sucursal ts, top_empresa tm
         WHERE ts.suc_cod = tm.suc_cod
          AND tm.emp_nomempresa = gar_det[ln_ind].emp_nomempresa}
        
        IF gs_existe_dato THEN
          LET gs_msn = "El Nombre de la Empresa ",gar_det[ln_ind].emp_nomempresa  CLIPPED , " ya existe en esta sucursal"--, gr_top_empresa.nomsucursal
          CALL fgl_winmessage("Informacion",gs_msn,"information")
          NEXT FIELD empnomempresa                  
        END IF        
      END IF 

    AFTER FIELD empdireccion
      LET ln_ind = arr_curr()
      IF (gar_det[ln_ind].emp_direccion IS NULL OR gar_det[ln_ind].emp_direccion = " ") THEN
        CALL fgl_winmessage("Informacion","Dede Digitar la Direccion de la Empresa","information")
        NEXT FIELD empdireccion
      END IF 

    AFTER FIELD emptelefono
      LET ln_ind = arr_curr()
      IF (gar_det[ln_ind].emp_telefono IS NULL OR gar_det[ln_ind].emp_telefono = " ") THEN
        CALL fgl_winmessage("Informacion","Debe Digitar el Telefono de la Empresa","information")
        NEXT FIELD emptelefono
      END IF 

    AFTER FIELD empgrupo
      LET ln_ind = arr_curr()
      IF (gar_det[ln_ind].emp_grupo IS NULL OR gar_det[ln_ind].emp_grupo = " ") THEN
        CALL fgl_winmessage("Informacion","pertence al Grupo","information")
        NEXT FIELD empgrupo
      END IF    
      
    BEFORE ROW
      LET ln_scrn = scr_line()
      IF ln_idx <= gn_max_scr THEN
        DISPLAY gr_top_empresa.emp_idempresa TO idempresa      
        --DISPLAY gar_det[ln_i].* TO scr_empresa[ln_scrn].*
      END IF
      CALL gar_det.getLength() RETURNING gn_max 
     AFTER ROW  
      CALL gar_det.getLength() RETURNING gn_max  

    END INPUT 

    ON ACTION SAVE
      CASE 
        WHEN gs_opcion = "A"     
          IF insertar_tipoempresa() THEN          
            CALL fgl_winmessage("Informacion","La informacion fue actualizada","information")            
          ELSE 
            CALL fgl_winmessage("Stop","La informacion no fue generada","stop")  
          END IF

        WHEN gs_opcion = "M"
               
          IF actualiza_tipoempresa() THEN
            CALL fgl_winmessage("Informacion","La informacion fue modificada","information")        
          ELSE
            CALL fgl_winmessage("Stop","La informacion no fue generada","Stop")
          END IF

        WHEN gs_opcion = "I" 
          IF insertar_empresa() THEN 
            CALL fgl_winmessage("Informacion","La informacion fue actualizada","information")
            CALL muestra_pantalla(ln_posactual) --JHGC 19/02/2014 MUESTRA EL ID
          ELSE 
            CALL fgl_winmessage("Stop","La Informacion No Fue Generada","stop") 
          END IF  
      END CASE
    EXIT DIALOG
    
    ON ACTION  CANCEL 
      CALL inicializar_todo()
      LET INT_FLAG = TRUE 
      EXIT DIALOG  
  END DIALOG 

  CALL gar_det.getLength() RETURNING gn_max
  IF INT_FLAG THEN 
    CALL fgl_winmessage("Informacion","El Usuario Cancelo El Proceso De Insertar Linea","information")
    LET INT_FLAG = FALSE 
    LET ln_ok = FALSE
  END IF 

  IF gs_opcion = "M" THEN
    CALL inicializar_todo()
  END IF 
  RETURN ln_ok
END FUNCTION 

{FUNCION ACTUALIZA TIPO EMPRESA Y EMPRESA}
FUNCTION actualiza_tipoempresa()
  DEFINE
        ln_act,
        ln_i,
        ln_detalle SMALLINT

  LET ln_act = TRUE 
  LET gs_act = TRUE 
  BEGIN WORK 
  WHENEVER ERROR CONTINUE
  
  UPDATE top_tipoempresa
     SET (top_tipoempresa.tip_nomtipoem) = (gr_top_tipoempresa.tip_nomtipoem) 
   WHERE top_tipoempresa.tip_idtipoem = gr_top_tipoempresa.tip_idtipoem       

  IF sqlca.sqlcode <> 0 THEN
    LET gs_msn = "No Actualiza el Tiopo de Empresa (Error",sqlca.sqlcode, ")"
    CALL fgl_winmessage("Modificar",gs_msn,"stop")

    LET ln_act = FALSE 
    LET gs_act = FALSE   ##JHGC 03032014   
  END IF
  
  IF ln_act THEN
    LET ln_detalle = 0
    INITIALIZE gr_top_empresa.* TO NULL 
    LET gr_top_empresa.tip_idtipoem = gr_top_tipoempresa.tip_idtipoem
    
    FOR ln_i = 1 TO gn_max
      LET gr_Top_empresa.emp_idempresa = gar_det[ln_i].emp_idempresa
      LET gr_top_empresa.suc_cod = gar_det[ln_i].suc_cod       
      LET gr_top_empresa.emp_nitempresa = gar_det[ln_i].emp_nitempresa
      LET gr_top_empresa.tip_idtipoem = gar_det[ln_i].tip_idtipoem
      LET gr_top_empresa.emp_nomempresa = gar_det[ln_i].emp_nomempresa    
      LET gr_top_empresa.emp_direccion = gar_det[ln_i].emp_direccion
      LET gr_top_empresa.emp_telefono = gar_det[ln_i].emp_telefono
      LET gr_top_empresa.emp_contacto = gar_det[ln_i].emp_contacto
      LET gr_top_empresa.emp_emailcontac = gar_det[ln_i].emp_emailcontac
      LET gr_top_empresa.emp_grupo = gar_det[ln_i].emp_grupo

      UPDATE top_empresa
         SET (top_empresa.suc_cod,
              top_empresa.emp_nitempresa,top_empresa.tip_idtipoem,
              top_empresa.emp_nomempresa,top_empresa.emp_direccion,
              top_empresa.emp_telefono,top_empresa.emp_contacto,
              top_empresa.emp_emailcontac,top_empresa.emp_grupo)=
             (gr_top_empresa.suc_cod,
              gr_top_empresa.emp_nitempresa,gr_top_empresa.tip_idtipoem,
              gr_top_empresa.emp_nomempresa,gr_top_empresa.emp_direccion,
              gr_top_empresa.emp_telefono,gr_top_empresa.emp_contacto,
              gr_top_empresa.emp_emailcontac,gr_top_empresa.emp_grupo)                       
       WHERE  top_empresa.emp_idempresa = gr_top_empresa.emp_idempresa
         
      IF sqlca.sqlcode <> 0 THEN 
        LET gs_msn = "No Actualizo Empresa (Error ",sqlca.sqlcode, ")"
        CALL fgl_winmessage("Modifiacar",gs_msn,"stop")
        LET ln_act = FALSE 
        LET ln_i = gn_max
      END IF           
    END FOR   
  END IF  

  WHENEVER ERROR STOP 

  IF ln_act THEN
    COMMIT WORK 
  ELSE 
    ROLLBACK WORK
  END IF 
  RETURN  ln_act
END FUNCTION  

FUNCTION construct_datos()
  DEFINE ln_ok SMALLINT
  LET ln_ok = TRUE 
  INITIALIZE gs_condicion TO NULL 

  WHILE TRUE 
    CONSTRUCT BY NAME gs_condicion ON tip_idtipoem, tip_nomtipoem

    IF INT_FLAG THEN 
      INITIALIZE gr_top_tipoempresa.* TO NULL 
      DISPLAY BY NAME gr_top_tipoempresa.*
      LET INT_FLAG = FALSE
      
      IF gs_opcion = "C" THEN
        CALL fgl_winmessage("Informacion","Consulta Cancelada Por El Usuario","information")
        RETURN FALSE 
      END IF      
    END IF 
    --VALIDA LOS CAMPOS DE IDPAIS Y NOMCIUDAD 
    {IF gs_condicion MATCHES "*1=1*" AND ln_ok THEN
      CALL fgl_winmessage("Consulta","Debe especificar filtro de consulta","exclamation")
      CONTINUE WHILE  
     END IF }

  EXIT WHILE   
  END WHILE 

  IF ln_ok THEN 
    CALL prepara_consulta() RETURNING gn_count_enc
    IF gn_count_enc = 0 THEN
      CALL fgl_winmessage("Consulta","NO Encontro registro con ese filtro de consulta","exclamation")
      CALL inicializar_todo() --*****
      INITIALIZE gr_top_tipoempresa.* TO NULL 
      DISPLAY gr_top_tipoempresa.*
      LET ln_ok = FALSE 
    END IF 
  END IF 
  RETURN ln_ok
END FUNCTION 

{FUNCION INSERTA TIPO EMPRESA Y EMPRESA}
FUNCTION insertar_tipoempresa() 
  DEFINE
        ln_ins,
        ln_i,
        ln_detalle SMALLINT
  LET ln_ins = TRUE  
  BEGIN WORK
  WHENEVER ERROR CONTINUE
  
  IF gs_opcion = "A" THEN 
    INSERT INTO top_tipoempresa(tip_nomtipoem) VALUES (gr_top_tipoempresa.tip_nomtipoem)
  END IF 
    
  IF sqlca.sqlcode <> 0 THEN
    LET gs_msn = "No Adiciono el Tipo de Empresa (Error ", sqlca.sqlcode CLIPPED, ")"
    CALL fgl_winmessage("Adicionar",gs_msn, "stop")
    LET ln_ins = FALSE 
  ELSE 
    LET gr_top_tipoempresa.tip_idtipoem = sqlca.sqlerrd[2] 
    DISPLAY BY NAME gr_top_tipoempresa.tip_idtipoem
  END IF

  INITIALIZE gr_top_empresa.* TO NULL
  
  LET gr_top_empresa.tip_idtipoem = gr_top_tipoempresa.tip_idtipoem
  
  IF ln_ins THEN
    LET ln_detalle = 0
    FOR ln_i = 1 TO gn_max
      IF (gar_det[ln_i].emp_nitempresa IS NOT NULL) THEN
      
      LET gr_top_empresa.emp_idempresa    = gar_det[ln_i].emp_idempresa
      LET gr_top_empresa.suc_cod          = gar_det[ln_i].suc_cod
      LET gr_top_empresa.emp_nitempresa   = gar_det[ln_i].emp_nitempresa
      LET gr_top_empresa.emp_nomempresa   = gar_det[ln_i].emp_nomempresa
      LET gr_top_empresa.emp_direccion    = gar_det[ln_i].emp_direccion
      LET gr_top_empresa.emp_telefono     = gar_det[ln_i].emp_telefono
      LET gr_top_empresa.emp_contacto     = gar_det[ln_i].emp_contacto
      LET gr_top_empresa.emp_emailcontac  = gar_det[ln_i].emp_emailcontac
      LET gr_top_empresa.emp_grupo        = gar_det[ln_i].emp_grupo
                 
      INSERT INTO top_empresa
                  (top_empresa.suc_cod,
                   top_empresa.emp_nitempresa,top_empresa.tip_idtipoem,
                   top_empresa.emp_nomempresa,top_empresa.emp_direccion,
                   top_empresa.emp_telefono,top_empresa.emp_contacto,
                   top_empresa.emp_emailcontac,top_empresa.emp_grupo)
           VALUES (gr_top_empresa.suc_cod,
                   gr_top_empresa.emp_nitempresa,gr_top_empresa.tip_idtipoem,
                   gr_top_empresa.emp_nomempresa,gr_top_empresa.emp_direccion,
                   gr_top_empresa.emp_telefono,gr_top_empresa.emp_contacto,
                   gr_top_empresa.emp_emailcontac,gr_top_empresa.emp_grupo)

        IF sqlca.sqlcode <> 0 THEN
          LET gs_msn = "NO Adiciono Empresa (Error ", sqlca.sqlcode, ")"
          CALL fgl_winmessage("Adicionar",gs_msn, "stop")
          LET ln_ins = FALSE
          LET ln_i = gn_max          
        ELSE
          LET gr_top_empresa.emp_idempresa = sqlca.sqlerrd[2];
          LET gar_det[ln_i].emp_idempresa = gr_top_empresa.emp_idempresa          
          DISPLAY gr_top_empresa.emp_idempresa TO idempresa  
          LET ln_detalle = ln_detalle + 1
        END IF
      END IF
    END FOR
  END IF
  WHENEVER ERROR STOP
  
  IF ln_ins THEN
    COMMIT WORK   
  ELSE
    ROLLBACK WORK  
  END IF
  RETURN ln_ins
END FUNCTION 

FUNCTION prepara_consulta()
  DEFINE ln_count INTEGER

  LET ln_count = 0
  INITIALIZE gs_query TO NULL

  LET gs_query = "select count(*) from top_tipoempresa where ", gs_condicion
  PREPARE p_count FROM gs_query
  DECLARE c_count CURSOR FOR p_count
  OPEN c_count
  FETCH c_count INTO ln_count
  CLOSE c_count

  IF (ln_count IS NULL OR ln_count = 0) THEN
    LET ln_count = 0
  ELSE
    IF gs_opcion = "C" THEN
      LET gs_query = "select *",
                     "  from top_tipoempresa",
                     " where ", gs_condicion CLIPPED,
                     " order by tip_idtipoem"
      PREPARE p_conslt_enc FROM gs_query
      DECLARE c_conslt_enc SCROLL CURSOR FOR p_conslt_enc

      LET gs_query = "select e.emp_idempresa,e.suc_cod,s.suc_descripcion,e.emp_nitempresa,
                             e.emp_nomempresa,e.emp_direccion,e.emp_telefono,
                             e.emp_contacto,e.emp_emailcontac,e.emp_grupo,
                             e.tip_idtipoem",
                     "  from top_empresa e, top_sucursal s",
                     " where tip_idtipoem = ?",
                     " and e.suc_cod = s.suc_cod",
                     " and e.suc_cod =  ", g_sucursal CLIPPED, 
                     " order by emp_idempresa " 
                     
      PREPARE p_conslt_det FROM gs_query
      DECLARE c_conslt_det SCROLL CURSOR FOR p_conslt_det
    END IF    
  END IF
  RETURN ln_count
END FUNCTION

FUNCTION recorre_consulta()
  DEFINE ln_posactual INTEGER,
         ls_sino CHAR(1)
  OPEN c_conslt_enc
  FETCH FIRST c_conslt_enc INTO gr_top_tipoempresa.*
  LET ln_posactual = 1
  CALL muestra_pantalla(ln_posactual)

  MENU --"CONSULTA" ATTRIBUTES (STYLE="default",COMMENT="Menu de Tipo Empresa [Consulta]",IMAGE="attention")

    ON ACTION nextrow
      LET gs_opcion = "C"
      
      FETCH NEXT c_conslt_enc INTO gr_top_tipoempresa.*
      IF sqlca.sqlcode = NOTFOUND THEN
        CALL fgl_winmessage("Consulta",
                            "No existen mas Registros en esa direccion.",
                            "information")
      ELSE
        LET ln_posactual = ln_posactual + 1
        CALL muestra_pantalla(ln_posactual)
      END IF

    ON ACTION prevrow
      LET gs_opcion = "C"
      FETCH PREVIOUS c_conslt_enc INTO gr_top_tipoempresa.*
      IF sqlca.sqlcode = NOTFOUND THEN
        CALL fgl_winmessage("Consulta",
                            "No existen mas Registros en esa direccion.",
                            "information")
      ELSE
        LET ln_posactual = ln_posactual - 1
        CALL muestra_pantalla(ln_posactual)
      END IF
    
    ON ACTION firstrow
      LET gs_opcion = "C"
      FETCH FIRST c_conslt_enc INTO gr_top_tipoempresa.*
      LET ln_posactual = 1
      CALL muestra_pantalla(ln_posactual)
    
    ON ACTION lastrow
      LET gs_opcion = "C"
      FETCH LAST c_conslt_enc INTO gr_top_tipoempresa.* 
      IF sqlca.sqlcode = NOTFOUND THEN
        CALL fgl_winmessage("Consulta",
                            "No existen mas Registros en esa direccion.",
                            "information")
      ELSE
        LET ln_posactual = gn_count_enc
        CALL muestra_pantalla(ln_posactual)
      END IF
      
    {MODIFICA TIPO EMPRESA O EMPRESA}
    ON ACTION MODIFY  
     
        LET gs_opcion = "M"
      
        IF input_tipoempresa() THEN
          
          IF gs_act THEN ##JHGC 03032014
            CALL fgl_winmessage("Informacion","El usario actualizo datos","Information")
          END IF  
          CLOSE c_conslt_enc
          OPEN c_conslt_enc
          FETCH ABSOLUTE ln_posactual c_conslt_enc INTO gr_top_tipoempresa.*
          CALL muestra_pantalla(ln_posactual)
          LET gs_opcion = "C"
        END IF 
    ##INSERTAR REGISTRO EN TOP_EMPRESA 'DETALLE'
    ON ACTION APPEND       
      LET gs_opcion = "I"      
      IF input_empresa() THEN       
        CLOSE c_conslt_enc
        OPEN c_conslt_enc
        FETCH ABSOLUTE ln_posactual c_conslt_enc INTO gr_top_tipoempresa.*
        CALL muestra_pantalla(ln_posactual)
      END IF 
      LET gs_opcion = "C"
 
    ON ACTION CANCEL
      CALL fgl_winmessage("Informacion","El Usuario Cancelo La Busqueda","information")
      CALL inicializar_todo()
      EXIT MENU 
 
  END MENU
END FUNCTION 

FUNCTION muestra_pantalla(ln_actual) 
  DEFINE
    ln_actual,ln_i SMALLINT
  
  IF gs_opcion = "C" THEN 
    CLEAR FORM
  END IF 
  
  DISPLAY BY NAME gr_top_tipoempresa.*
  LET gr_top_tipoempresaaux.* = gr_top_tipoempresa.*
  CALL carga_det_pantalla()
  
  CALL set_count(gn_max) 
  DISPLAY ARRAY gar_det TO scr_empresa.* ATTRIBUTES (COUNT = gn_max)
    BEFORE DISPLAY
    EXIT DISPLAY
  END DISPLAY
  
  FOR ln_i = 1 TO gn_max
    IF ln_i <= gn_max_scr THEN
      DISPLAY gar_det[ln_i].* TO scr_empresa[ln_i].*
    END IF
  END FOR  

  MESSAGE "(Tipo Empresa : ", ln_actual USING "###", "/", gn_count_enc USING "###", ")",
          " (Empresas : ", gn_max USING "###", ")"
END FUNCTION

FUNCTION carga_det_pantalla()
  INITIALIZE gr_det.* TO NULL
  CALL gar_det.clear()
  CALL gar_detaux.clear()
 
  IF gs_opcion != "A" THEN 
    
    FOREACH c_conslt_det USING gr_top_tipoempresa.tip_idtipoem INTO gr_det.*
      CALL gar_det.appendElement()
      LET gar_det[gar_det.getLength()].* = gr_det.*
      CALL gar_detaux.appendElement()
      LET gar_detaux[gar_detaux.getLength()].* = gr_det.*
    END FOREACH
    CALL gar_det.getLength() RETURNING gn_max
  END IF   
END FUNCTION

{INSERTAR EMPRESA}
FUNCTION insertar_empresa()

  DEFINE ln_ins,
         ln_i,
         ln_detalle SMALLINT
  
  LET ln_ins = TRUE

  BEGIN WORK
  WHENEVER ERROR CONTINUE

  INITIALIZE gr_top_empresa.* TO NULL
  
  LET gr_top_empresa.tip_idtipoem = gr_top_tipoempresa.tip_idtipoem
  
  FOR ln_i = 1 TO gn_max               
    IF (gar_det[ln_i].emp_idempresa IS NULL) THEN     
      LET gr_top_empresa.suc_cod          = gar_det[ln_i].suc_cod
      LET gr_top_empresa.emp_nitempresa   = gar_det[ln_i].emp_nitempresa
      LET gr_top_empresa.emp_nomempresa   = gar_det[ln_i].emp_nomempresa
      LET gr_top_empresa.emp_direccion    = gar_det[ln_i].emp_direccion
      LET gr_top_empresa.emp_telefono     = gar_det[ln_i].emp_telefono
      LET gr_top_empresa.emp_contacto     = gar_det[ln_i].emp_contacto
      LET gr_top_empresa.emp_emailcontac  = gar_det[ln_i].emp_emailcontac
      LET gr_top_empresa.emp_grupo        = gar_det[ln_i].emp_grupo
                 
      INSERT INTO top_empresa
                  (top_empresa.suc_cod,
                   top_empresa.emp_nitempresa,top_empresa.tip_idtipoem,
                   top_empresa.emp_nomempresa,top_empresa.emp_direccion,
                   top_empresa.emp_telefono,top_empresa.emp_contacto,
                   top_empresa.emp_emailcontac,top_empresa.emp_grupo)
           VALUES (gr_top_empresa.suc_cod,
                   gr_top_empresa.emp_nitempresa,gr_top_empresa.tip_idtipoem,
                   gr_top_empresa.emp_nomempresa,gr_top_empresa.emp_direccion,
                   gr_top_empresa.emp_telefono,gr_top_empresa.emp_contacto,
                   gr_top_empresa.emp_emailcontac,gr_top_empresa.emp_grupo)
        
      IF sqlca.sqlcode <> 0 THEN        
        LET gs_msn = "NO Adiciono Empresa (Error ", sqlca.sqlcode, ")"
        CALL fgl_winmessage("Adicionar",gs_msn, "stop")
        LET ln_ins = FALSE
        LET ln_i = gn_max
      ELSE
        LET ln_detalle = ln_detalle + 1
      END IF
    END IF
  END FOR  

  WHENEVER ERROR STOP
  
  IF ln_ins THEN
    COMMIT WORK
  ELSE
    ROLLBACK WORK  
  END IF
  RETURN ln_ins
END FUNCTION 

FUNCTION existe_dato(l_tabla,l_campo,l_var)
  DEFINE
        l_query, l_tabla CHAR (200),
        l_campo CHAR (50),
        l_var LIKE top_empresa.emp_nomempresa,
        l_contador INTEGER 

  LET l_contador = 0
  LET l_query = "SELECT COUNT(*) FROM ", l_tabla CLIPPED,
                              " WHERE ", l_campo CLIPPED, " MATCHES '*",l_var CLIPPED, "*'", --MATCHES '*
                              " AND suc_cod = ", g_sucursal
  PREPARE ext_sql FROM l_query 
  DECLARE ext_sql_emp2 CURSOR FOR ext_sql

  FOREACH ext_sql_emp2 INTO l_contador
    IF l_contador > 0 THEN 
      LET gs_existe_dato = TRUE 
    ELSE 
      LET gs_existe_dato = FALSE 
    END IF
  END FOREACH
  RETURN gs_existe_dato 
END FUNCTION 

FUNCTION existe_dato_(l_tabla,l_campo,l_campo1,l_var,l_var1)
  DEFINE
        l_query, l_tabla CHAR (200),
        l_campo,l_campo1, l_var, l_var1 CHAR (100),
        l_contador INTEGER 

  LET l_contador = 0
  CASE 
    WHEN gs_prm = "nit" 
      LET l_query = "SELECT COUNT(*) FROM ", l_tabla CLIPPED,
                                  " WHERE ", l_campo CLIPPED, " = '",l_var CLIPPED,"'",
                                    " AND ", l_campo1 CLIPPED," = ",l_var1 CLIPPED
    WHEN gs_prm = "nom"
      LET l_query = "SELECT COUNT(*) FROM ", l_tabla CLIPPED,
                                  " WHERE ", l_campo CLIPPED, " MATCHES '*",l_var CLIPPED,"*'",
                                    " AND ", l_campo1 CLIPPED," = ",l_var1 CLIPPED 
  END CASE 
  
  PREPARE ext_sql1 FROM l_query 
  DECLARE ext_sql_emp3 CURSOR FOR ext_sql1

  FOREACH ext_sql_emp3 INTO l_contador 
    IF l_contador > 0 THEN 
      LET gs_existe_dato = TRUE 
    ELSE 
      LET gs_existe_dato = FALSE 
    END IF
  END FOREACH 
  RETURN gs_existe_dato 
END FUNCTION 
 
{##FUNCION PARA DESPLEGAR EL ZOOM DE LAS SUCURSALES
FUNCTION make_help_emp(l_tabname,l_primkey,l_description,l_titulo,l_where_info)
DEFINE lr_help RECORD
       codigo varchar(15),
       descr  varchar(80)
END RECORD
DEFINE l_where_info STRING
DEFINE l_tabname char(128)
DEFINE l_primkey char(128)
DEFINE l_description STRING
DEFINE l_titulo,l_query STRING
DEFINE l_arr_help DYNAMIC ARRAY OF RECORD
    codigo  varchar(15),
    descr   varchar(80)
    END RECORD
DEFINE lr_filtro RECORD
       campo    varchar(20),
       filtro   varchar(80)
END RECORD

IF l_where_info IS NULL OR l_where_info="" THEN
  LET l_query="SELECT ",l_primkey CLIPPED,",",l_description CLIPPED,
  " FROM ",l_tabname CLIPPED
ELSE
  LET l_query="SELECT ",l_primkey CLIPPED,",",l_description CLIPPED,
  " FROM ",l_tabname CLIPPED," WHERE ",l_where_info CLIPPED
END IF
PREPARE ext_sql_emp FROM l_query
DECLARE lc_help CURSOR FOR ext_sql_emp

--CALL l_arr_help.appendElement()
CALL l_arr_help.clear()

FOREACH lc_help INTO lr_help.*
  CALL l_arr_help.appendElement()
  LET l_arr_help[l_arr_help.getLength()].* = lr_help.*
END FOREACH

IF l_arr_help.getLength() = 0 THEN
  CALL fgl_winmessage("Consulta","No existen Registros!", "information")
  RETURN NULL
END IF

OPEN WINDOW w_help WITH FORM "fhelp_emp"
CALL fgl_settitle(l_titulo)

DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)
  INPUT BY NAME lr_filtro.*
  
    BEFORE INPUT
      LET lr_filtro.campo="DESCRIPCION"
      DISPLAY BY NAME lr_filtro.campo
      NEXT FIELD filtro
      AFTER FIELD filtro
      CALL l_arr_help.clear()
      IF lr_filtro.campo="CODIGO" THEN
        LET l_query="SELECT ",l_primkey CLIPPED,",",l_description CLIPPED,
        " FROM ",l_tabname CLIPPED, " WHERE '", l_primkey CLIPPED," = '",
        lr_filtro.campo CLIPPED,"'"
      ELSE
        LET l_query="SELECT ",l_primkey CLIPPED,",",l_description CLIPPED,
        " FROM ",l_tabname CLIPPED, " WHERE ", l_description CLIPPED," matches '*",
        lr_filtro.filtro CLIPPED,"*'"
      END IF
      IF l_where_info IS NOT NULL THEN
        LET l_query=l_query, " AND ",l_where_info
      END IF 

      PREPARE ext_sql1 FROM l_query
      DECLARE lc_help1 CURSOR FOR ext_sql1
      CALL l_arr_help.clear()

      FOREACH lc_help1 INTO lr_help.*
        CALL l_arr_help.appendElement()
        LET l_arr_help[l_arr_help.getLength()].* = lr_help.*
      END FOREACH

      IF l_arr_help.getLength() = 0 THEN
        CALL fgl_winmessage("Consulta","No existen Registros para ese Parametro!", "information")
        NEXT FIELD filtro
      END IF
      NEXT FIELD codigo
  END INPUT
  DISPLAY ARRAY l_arr_help TO table2.*
    ON ACTION ACCEPT
      RETURN l_arr_help[arr_curr()].codigo
  END DISPLAY
  ON ACTION CANCEL
    RETURN NULL
  BEFORE DIALOG
    NEXT FIELD campo
END DIALOG
END FUNCTION}