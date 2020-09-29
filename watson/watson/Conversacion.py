from ibm_watson import AssistantV2
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator
from ibm_watson import ApiException
from datetime import datetime
import mysql.connector
import json

class Conversacion:



    def __init__(self):
        #API Watson
        self.assistant = AssistantV2(
            version='2020-04-01',
            authenticator=IAMAuthenticator('qKTxZfdEDYO3HLcTmsAvzQgNthvF2T1T3RBbe0_2m3js')
        )
        self.assistant.set_service_url('https://api.eu-gb.assistant.watson.cloud.ibm.com/instances/62281458-78dc-4956-8c27-589b3508a8c4')
        self.assistant.set_disable_ssl_verification(True)

        #Id de Asistente Watson
        self.assistant_id='b56fd503-f8be-42b2-846c-ed01ebc37b3f'
        
        #Configuracion MySQL
        self.cnx = mysql.connector.connect(
            host="localhost",
            user="root",
            password="mysqlubuntu",
            database="CHATBOT_CUENTA_NEGOCIO"
        )

    def enviarMensaje(self,numero,mensaje):
        sesion = self.getSesion(numero)
        response = self.assistant.message(
            assistant_id=self.assistant_id,
            session_id=sesion,
            input={
                'message_type': 'text',
                'text': mensaje
            }).get_result()
        
        self.registrarConversacion(response,self.assistant_id,sesion,mensaje,numero)
        return response['output']['generic'][0]['text']

    def registrarConversacion(self,response,assistant_id,session_id,mensaje,numero):
        today = datetime.now()
        cursor = self.cnx.cursor()
        agregar = "INSERT INTO CONVERSACION(SESSION_ID,ASSISTANT_ID,CLIENTE,FECHA,TEXTO_CLIENTE,TEXTO_RESPUESTA,INTENCIONES,ENTIDADES) VALUES (%s,%s,%s,%s,%s,%s,%s,%s);"
        cursor.execute(agregar, (session_id,assistant_id,numero,today.strftime("%Y-%m-%d %H:%M:%S"),mensaje
            ,response['output']['generic'][0]['text']
            ,json.dumps(response['output']['intents'])
            ,json.dumps(response['output']['entities'])
            ))
        self.cnx.commit()

    
    def getSesion(self,numero):
        #Buscamos en BD si ya existe una sesion previa
        query = ("SELECT TELEFONO,SESION FROM WATSON_SESSION WHERE TELEFONO = %s AND TIME_TO_SEC(TIMEDIFF(NOW(), FECHA)) < 280")
        cursor = self.cnx.cursor()
        cursor.execute(query, (numero,))

        sesion = None
        for (TELEFONO,SESION) in cursor:
            sesion = SESION

        #Si no hay sesion, obtenemos una de watson y la guardamos
        if (sesion == None):
            resp = self.assistant.create_session(
                assistant_id=self.assistant_id
            ).get_result()
            sesion = resp['session_id']

            today = datetime.now()

            agregar = "INSERT INTO WATSON_SESSION VALUES (%s,%s,%s);"
            cursor.execute(agregar, (numero,sesion,today.strftime("%Y-%m-%d %H:%M:%S")))
            self.cnx.commit()
        
        return sesion

    
