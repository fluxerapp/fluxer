// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_ES_ES_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Tu cuenta de {product_name} ha sido deshabilitada temporalmente",
		"body": "Hola {username}:\n\nHemos deshabilitado temporalmente tu cuenta de {product_name} porque hemos detectado actividad sospechosa.\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nPara recuperar el acceso a tu cuenta, tendrás que restablecer tu contraseña:\n\n{forgotUrl}\n\nDespués de restablecer tu contraseña, podrás iniciar sesión de nuevo.\n\nSi crees que esto ha sido un error, ponte en contacto con nuestro equipo de soporte.\n\n– Equipo de Seguridad de {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Tu cuenta de {product_name} será eliminada permanentemente",
		"body": "Hola {username}:\n\nTu cuenta de {product_name} ha sido programada para su eliminación permanente debido a infracciones de nuestros Términos de Servicio o Directrices de la Comunidad.\n\nEliminación programada: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nEsta es una medida grave. Los datos de tu cuenta se eliminarán permanentemente en la fecha programada.\n\nPor favor, revisa:\n- Términos de Servicio: {termsUrl}\n- Directrices de la Comunidad: {guidelinesUrl}\n\nProceso de apelación:\nSi crees que esta decisión fue incorrecta o injustificada, tienes 60 días para presentar una apelación. Envía un correo electrónico a {appeals_email} desde esta dirección de correo electrónico.\n\nEn tu apelación:\n- Explica claramente por qué crees que la decisión fue incorrecta o injustificada\n- Proporciona cualquier evidencia o contexto relevante\n\nUn miembro del Equipo de Seguridad de {product_name} revisará tu apelación y podrá pausar la eliminación pendiente hasta que se haya tomado una decisión final.\n\n– Equipo de Seguridad de {product_name}"
	},
	"account_temp_banned": {
		"subject": "Tu cuenta de {product_name} ha sido suspendida temporalmente",
		"body": "Hola {username}:\n\nTu cuenta de {product_name} ha sido suspendida temporalmente por violar nuestros Términos de Servicio o Directrices de la Comunidad.\n\nDuración: {durationHours, plural,\n  =1 {1 hora}\n  other {# horas}\n}\nSuspendida hasta: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nDurante este tiempo, no podrás acceder a tu cuenta.\n\nPor favor, revisa:\n- Términos de Servicio: {termsUrl}\n- Directrices de la Comunidad: {guidelinesUrl}\n\nSi crees que esta decisión fue incorrecta o injustificada, puedes presentar una apelación. Envía un correo electrónico a {appeals_email} desde esta dirección de correo electrónico y explica claramente por qué crees que la decisión fue incorrecta. Revisaremos tu apelación y te responderemos con nuestra decisión.\n\n– Equipo de Seguridad de {product_name}"
	},
	"donation_confirmation": {
		"subject": "Gracias por tu donación a {product_name}",
		"body": "Hola:\n\n¡Gracias por tu donación a {product_name}! Tu {interval, select,\n  month {donación recurrente}\n  year {donación recurrente}\n  other {donación única}\n} se ha {interval, select,\n  month {configurado}\n  year {configurado}\n  other {procesado}\n} correctamente.\n\nDetalles de la donación:\nCantidad: {amount} {currency} {interval, select,\n  month {al mes}\n  year {al año}\n  other {}\n}\n\nStripe te enviará un recibo separado con tu factura en PDF en breve. Este recibo incluye todos los detalles de pago y se puede utilizar para fines fiscales.\n\nPuedes ver tu historial de donaciones, descargar facturas {interval, select,\n  month {y gestionar o cancelar tu suscripción}\n  year {y gestionar o cancelar tu suscripción}\n  other {y gestionar futuras donaciones}\n} en cualquier momento usando este enlace:\n\n{manageUrl}\n\nTu apoyo ayuda a mantener {product_name} en funcionamiento. ¡Gracias!\n\n– Equipo de {product_name}"
	},
	"donation_magic_link": {
		"subject": "Gestiona tus donaciones de {product_name}",
		"body": "Hola:\n\nHaz clic en el siguiente enlace para acceder a tu portal de donantes:\n\n{manageUrl}\n\nEn el portal, puedes gestionar suscripciones, descargar facturas y ver tu historial de donaciones.\n\nEste enlace caduca el {expiresAt, date, full} a las {expiresAt, time, short}.\n\nSi no solicitaste este enlace, puedes ignorar este correo electrónico de forma segura.\n\n– Equipo de {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Verifica tu correo electrónico para un informe de la DSA",
		"body": "Hola:\n\nUsa el siguiente código de verificación para enviar tu informe de la Ley de Servicios Digitales en {product_name}:\n\n{code}\n\nEste código caduca el {expiresAt, date, full} a las {expiresAt, time, short}.\n\nSi no solicitaste esto, puedes ignorar este correo electrónico.\n\n– Equipo de Seguridad de {product_name}"
	},
	"email_change_new": {
		"subject": "Verifica tu nuevo correo electrónico para {product_name}",
		"body": "Hola {username}:\n\nIntroduce este código en la aplicación para verificar tu nuevo correo electrónico para {product_name}:\n\n{code}\n\nEste código caduca el {expiresAt, date, full} a las {expiresAt, time, short}.\n\nSi no solicitaste esto, puedes ignorar este correo electrónico.\n\n– Equipo de {product_name}"
	},
	"email_change_original": {
		"subject": "Confirma tu cambio de correo electrónico para {product_name}",
		"body": "Hola {username}:\n\nHemos recibido una solicitud para cambiar la dirección de correo electrónico de tu cuenta de {product_name}.\n\nPara confirmar este cambio, introduce este código en la aplicación:\n\n{code}\n\nEste código caduca el {expiresAt, date, full} a las {expiresAt, time, short}.\n\nSi no solicitaste esto, protege tu cuenta de inmediato.\n\n– Equipo de {product_name}"
	},
	"email_change_revert": {
		"subject": "Tu correo electrónico para {product_name} ha sido cambiado",
		"body": "Hola {username}:\n\nLa dirección de correo electrónico de tu cuenta de {product_name} ha sido cambiada a {newEmail}.\n\nSi realizaste este cambio, no es necesario que hagas nada. Si no lo hiciste, puedes revertir el cambio y proteger tu cuenta usando este enlace:\n\n{revertUrl}\n\nEsto restaurará tu correo electrónico anterior, cerrará tu sesión en todas partes, eliminará los números de teléfono vinculados, deshabilitará la autenticación multifactor y te pedirá que establezcas una nueva contraseña.\n\n– Equipo de Seguridad de {product_name}"
	},
	"email_verification": {
		"subject": "Verifica tu dirección de correo electrónico para {product_name}",
		"body": "Hola {username}:\n\nVerifica la dirección de correo electrónico de tu cuenta de {product_name} haciendo clic en el siguiente enlace:\n\n{verifyUrl}\n\nSi no creaste una cuenta de {product_name}, puedes ignorar este correo electrónico.\n\nEste enlace es válido durante 24 horas.\n\n– Equipo de {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Se han eliminado las ventajas obtenidas con tu regalo canjeado",
		"body": "Hola {username}:\n\nUn código de regalo que canjeaste fue pagado originalmente por otra persona. Ese pago ha sido revertido (una devolución de cargo).\n\nDebido a esto, hemos eliminado las ventajas que se añadieron a tu cuenta cuando canjeaste el regalo.\n\nSi crees que esto es un error, ponte en contacto con nuestro equipo de soporte e incluye cualquier detalle que tengas sobre el código de regalo y cuándo lo canjeaste.\n\n– Equipo de {product_name}"
	},
	"harvest_completed": {
		"subject": "Tu exportación de datos de {product_name} está lista para descargar",
		"body": "Hola {username}:\n\nTu exportación de datos está lista.\n\nEnlace de descarga:\n{downloadUrl}\n\nMensajes incluidos: {totalMessages, number}\nTamaño del archivo: {fileSizeMB, number} MB\n\nEste enlace caduca el {expiresAt, date, full} a las {expiresAt, time, short}.\n\nSi no solicitaste esta exportación, cambia tu contraseña inmediatamente y ponte en contacto con nuestro equipo de soporte.\n\n– Equipo de {product_name}"
	},
	"inactivity_warning": {
		"subject": "Tu cuenta de {product_name} será eliminada debido a la inactividad",
		"body": "Hola {username}:\n\nNo hemos visto ninguna actividad en tu cuenta de {product_name} desde el {lastActiveDate, date, full}.\n\nSi no inicias sesión antes del {deletionDate, date, full} a las {deletionDate, time, short}, tu cuenta será eliminada permanentemente debido a la inactividad.\n\nInicia sesión aquí:\n{loginUrl}\n\nSi has usado {product_name} recientemente, ponte en contacto con nuestro equipo de soporte de inmediato.\n\n– Equipo de {product_name}"
	},
	"ip_authorization": {
		"subject": "Autorizar inicio de sesión desde una nueva dirección IP",
		"body": "Hola {username}:\n\nHemos detectado un intento de inicio de sesión en tu cuenta de {product_name} desde una nueva IP:\n\nDirección IP: {ipAddress}\nUbicación: {location}\n\nSi fuiste tú, autoriza esta dirección IP haciendo clic en el siguiente enlace:\n\n{authUrl}\n\nSi no intentaste iniciar sesión, cambia tu contraseña de inmediato.\n\nEste enlace es válido durante 30 minutos.\n\n– Equipo de {product_name}"
	},
	"password_change_verification": {
		"subject": "Confirma tu cambio de contraseña en {product_name}",
		"body": "Hola {username}:\n\nHemos recibido una solicitud para cambiar la contraseña de tu cuenta de {product_name}.\n\nPara confirmar este cambio, introduce este código en la aplicación:\n\n{code}\n\nEste código caduca el {expiresAt}.\n\nSi no solicitaste esto, alguien podría tener acceso a tu cuenta. Cambia tu contraseña inmediatamente y activa la autenticación de dos factores.\n\n– Equipo de {product_name}"
	},
	"password_reset": {
		"subject": "Restablece tu contraseña en {product_name}",
		"body": "Hola {username}:\n\nSolicitaste un restablecimiento de contraseña de {product_name}. Usa el siguiente enlace para establecer una nueva contraseña:\n\n{resetUrl}\n\nSi no solicitaste esto, puedes ignorar este correo electrónico.\n\nEste enlace es válido durante 1 hora.\n\n– Equipo de {product_name}"
	},
	"registration_approved": {
		"subject": "Tu registro en {product_name} ha sido aprobado",
		"body": "Hola {username}:\n\nBuenas noticias: tu registro en {product_name} ha sido aprobado.\n\nAhora puedes iniciar sesión en la aplicación de {product_name} aquí:\n{channelsUrl}\n\nBienvenido a la comunidad de {product_name}.\n\n– Equipo de {product_name}"
	},
	"report_resolved": {
		"subject": "Tu informe en {product_name} ha sido revisado",
		"body": "Hola {username}:\n\nTu informe (ID: {reportId}) ha sido revisado por nuestro Equipo de Seguridad.{hasComment, select, yes {\n\nRespuesta del Equipo de Seguridad:\n{publicComment}} other {}}\n\nGracias por ayudarnos a mantener {product_name} seguro para todos. Nos tomamos todos los informes en serio y agradecemos tu contribución a la comunidad.\n\nSi tienes alguna pregunta o inquietud sobre este resultado, ponte en contacto con {safety_email}.\n\n– Equipo de Seguridad de {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Tu cuenta de {product_name} será eliminada permanentemente",
		"body": "Hola {username}:\n\nTu cuenta de {product_name} ha sido programada para su eliminación permanente.\n\nEliminación programada: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nEsta es una medida grave. Los datos de tu cuenta se eliminarán permanentemente en la fecha programada.\n\nSi crees que esta decisión fue incorrecta, puedes presentar una apelación. Envía un correo electrónico a {appeals_email} desde esta dirección de correo electrónico.\n\n– Equipo de Seguridad de {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "La eliminación de tu cuenta de {product_name} está programada",
		"body": "Hola {username}:\n\nSolicitaste eliminar tu cuenta de {product_name}. Tu cuenta está programada para su eliminación permanente el:\n\n{deletionDate, date, full} a las {deletionDate, time, short}\n\nSi no solicitaste esto, inicia sesión en tu cuenta para cancelar la eliminación. También te recomendamos cambiar tu contraseña para proteger tu cuenta.\n\n– Equipo de {product_name}"
	},
	"unban_notification": {
		"subject": "Se ha levantado la suspensión de tu cuenta de {product_name}",
		"body": "Hola {username}:\n\nBuenas noticias: se ha levantado la suspensión de tu cuenta de {product_name}.\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nAhora puedes volver a iniciar sesión y seguir usando {product_name} con normalidad.\n\n– Equipo de Seguridad de {product_name}"
	}
});

export default EMAIL_I18N_ES_ES_MESSAGES;
