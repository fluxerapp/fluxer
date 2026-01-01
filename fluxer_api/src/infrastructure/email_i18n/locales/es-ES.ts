/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmailTranslations} from '../types';

export const esES: EmailTranslations = {
	passwordReset: {
		subject: 'Restablece tu contraseña de Fluxer',
		body: `Hola {username},

Has solicitado restablecer la contraseña de tu cuenta de Fluxer. Por favor, sigue el enlace de abajo para establecer una nueva contraseña:

{resetUrl}

Si no solicitaste este restablecimiento de contraseña, puedes ignorar este correo de forma segura.

Este enlace expirará en 1 hora.

- Equipo de Fluxer`,
	},
	emailVerification: {
		subject: 'Verifica tu dirección de correo electrónico de Fluxer',
		body: `Hola {username},

Por favor verifica la dirección de correo electrónico de tu cuenta de Fluxer haciendo clic en el siguiente enlace:

{verifyUrl}

Si no creaste una cuenta de Fluxer, puedes ignorar este correo de forma segura.

Este enlace expirará en 24 horas.

- Equipo de Fluxer`,
	},
	ipAuthorization: {
		subject: 'Autoriza el inicio de sesión desde una nueva dirección IP',
		body: `Hola {username},

Detectamos un intento de inicio de sesión en tu cuenta de Fluxer desde una nueva dirección IP:

Dirección IP: {ipAddress}
Ubicación: {location}

Si fuiste tú, autoriza esta dirección IP haciendo clic en el enlace siguiente:

{authUrl}

Si no intentaste iniciar sesión, por favor cambia tu contraseña inmediatamente.

Este enlace de autorización expirará en 30 minutos.

- Equipo de Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Tu cuenta de Fluxer ha sido desactivada temporalmente',
		body: `Hola {username},

Tu cuenta de Fluxer ha sido desactivada temporalmente debido a actividad sospechosa.

{reason, select,
	null {}
	other {Motivo: {reason}

}}Para recuperar el acceso a tu cuenta, debes restablecer tu contraseña:

{forgotUrl}

Después de restablecer tu contraseña, podrás iniciar sesión nuevamente.

Si crees que esta acción fue un error, por favor contacta a nuestro equipo de soporte.

- Equipo de Seguridad de Fluxer`,
	},
	accountTempBanned: {
		subject: 'Tu cuenta de Fluxer ha sido suspendida temporalmente',
		body: `Hola {username},

Tu cuenta de Fluxer ha sido suspendida temporalmente por violar nuestros Términos de Servicio o Normas de la Comunidad.

Duración: {durationHours, plural,
	=1 {1 hora}
	other {# horas}
}
Suspendido hasta: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Motivo: {reason}}
}

Durante este tiempo, no podrás acceder a tu cuenta.

Te recomendamos revisar:
- Términos de Servicio: {termsUrl}
- Normas de la Comunidad: {guidelinesUrl}

Si crees que esta decisión fue incorrecta o injustificada, puedes enviar una apelación a appeals@fluxer.app desde esta dirección de correo electrónico. Explica claramente por qué consideras que la decisión fue errónea. Revisaremos tu apelación y te responderemos con nuestra resolución.

- Equipo de Seguridad de Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Tu cuenta de Fluxer está programada para eliminación',
		body: `Hola {username},

Tu cuenta de Fluxer ha sido programada para eliminación permanente debido a violaciones de nuestros Términos de Servicio o Normas de la Comunidad.

Fecha de eliminación programada: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Motivo: {reason}}
}

Esta es una acción de cumplimiento seria. Los datos de tu cuenta serán eliminados permanentemente en la fecha programada.

Te recomendamos revisar:
- Términos de Servicio: {termsUrl}
- Normas de la Comunidad: {guidelinesUrl}

PROCESO DE APELACIÓN:
Si consideras que esta decisión fue incorrecta o injustificada, tienes 30 días para enviar una apelación a appeals@fluxer.app desde esta dirección de correo electrónico.

En tu apelación:
- Explica claramente por qué consideras que la decisión es incorrecta o injustificada
- Proporciona cualquier evidencia o contexto relevante

Un miembro del Equipo de Seguridad de Fluxer revisará tu apelación y podrá detener la eliminación programada hasta que se tome una decisión final.

- Equipo de Seguridad de Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'Se ha programado la eliminación de tu cuenta de Fluxer',
		body: `Hola {username},

¡Lamentamos que te vayas! La eliminación de tu cuenta de Fluxer ha sido programada.

Fecha de eliminación programada: {deletionDate, date, full} {deletionDate, time, short}

IMPORTANTE: Puedes cancelar esta eliminación en cualquier momento antes de {deletionDate, date, full} {deletionDate, time, short} simplemente iniciando sesión nuevamente en tu cuenta.

ANTES DE IRTE:
Tu Panel de Privacidad en la Configuración de Usuario te permite:
- Eliminar tus mensajes en la plataforma
- Extraer cualquier dato importante antes de irte

Ten en cuenta: Una vez que tu cuenta se elimine, no habrá forma de eliminar tus mensajes. Si deseas borrar tus mensajes, hazlo desde el Panel de Privacidad antes de que se complete la eliminación.

Si cambias de opinión, simplemente inicia sesión nuevamente para cancelar la eliminación.

- Equipo de Fluxer`,
	},
	inactivityWarning: {
		subject: 'Tu cuenta de Fluxer será eliminada por inactividad',
		body: `Hola {username},

Notamos que no has iniciado sesión en tu cuenta de Fluxer por más de 2 años.

Último inicio de sesión: {lastActiveDate, date, full} {lastActiveDate, time, short}

Como parte de nuestra política de retención de datos, las cuentas inactivas se programan automáticamente para eliminación. Tu cuenta será eliminada permanentemente el:

Fecha de eliminación programada: {deletionDate, date, full} {deletionDate, time, short}

CÓMO CONSERVAR TU CUENTA:
Simplemente inicia sesión en tu cuenta en {loginUrl} antes de la fecha de eliminación para cancelar esta eliminación automática. No se requiere ninguna otra acción.

¿QUÉ SUCEDE SI NO INICIAS SESIÓN?
- Tu cuenta y todos los datos asociados serán eliminados permanentemente
- Tus mensajes serán anonimizados (atribuidos a “Usuario Eliminado”)
- Esta acción no se puede revertir

¿QUIERES ELIMINAR TUS MENSAJES?
Si deseas eliminar tus mensajes antes de que tu cuenta sea eliminada, inicia sesión y utiliza el Panel de Privacidad en Configuración de Usuario.

¡Esperamos verte de vuelta en Fluxer!

- Equipo de Fluxer`,
	},
	harvestCompleted: {
		subject: 'Tu exportación de datos de Fluxer está lista',
		body: `Hola {username},

¡Tu exportación de datos ha sido completada y está lista para descargarse!

Resumen de exportación:
- Total de mensajes: {totalMessages, number}
- Tamaño del archivo: {fileSizeMB} MB
- Formato: Archivo ZIP con archivos JSON

Descarga tus datos: {downloadUrl}

IMPORTANTE: Este enlace de descarga expirará el {expiresAt, date, full} {expiresAt, time, short}

Lo que incluye tu exportación:
- Todos tus mensajes organizados por canal
- Metadatos de los canales
- Tu perfil de usuario e información de la cuenta
- Membresías y ajustes de guilds
- Sesiones de autenticación e información de seguridad

Los datos están organizados en formato JSON para facilitar su análisis.

Si tienes alguna pregunta sobre tu exportación de datos, contacta a support@fluxer.app

- Equipo de Fluxer`,
	},
	unbanNotification: {
		subject: 'La suspensión de tu cuenta de Fluxer ha sido levantada',
		body: `Hola {username},

¡Buenas noticias! La suspensión de tu cuenta de Fluxer ha sido levantada.

Motivo: {reason}

Ya puedes iniciar sesión nuevamente y continuar usando Fluxer.

- Equipo de Seguridad de Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Tu cuenta de Fluxer está programada para eliminación',
		body: `Hola {username},

Tu cuenta de Fluxer ha sido programada para eliminación permanente.

Fecha de eliminación programada: {deletionDate, date, full} {deletionDate, time, short}
Motivo: {reason}

Esta es una acción seria de cumplimiento. Los datos de tu cuenta serán eliminados permanentemente en la fecha programada.

Si crees que esta decisión fue incorrecta, puedes enviar una apelación a appeals@fluxer.app desde esta dirección de correo electrónico.

- Equipo de Seguridad de Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Tu regalo de Fluxer Premium ha sido revocado',
		body: `Hola {username},

Te informamos que el regalo de Fluxer Premium que canjeaste ha sido revocado debido a una disputa de pago (chargeback) presentada por el comprador original.

Tus beneficios premium han sido eliminados de tu cuenta. Esta acción se tomó porque el pago del regalo fue disputado y revertido.

Si tienes preguntas sobre esto, contacta a support@fluxer.app.

- Equipo de Fluxer`,
	},
	reportResolved: {
		subject: 'Tu reporte en Fluxer ha sido revisado',
		body: `Hola {username},

Tu reporte (ID: {reportId}) ha sido revisado por nuestro Equipo de Seguridad.

Respuesta del Equipo de Seguridad:
{publicComment}

Gracias por ayudar a mantener Fluxer seguro para todos. Tomamos todos los reportes en serio y apreciamos tu contribución a nuestra comunidad.

Si tienes preguntas o inquietudes sobre esta resolución, contacta a safety@fluxer.app.

- Equipo de Seguridad de Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Verifica tu correo para un reporte DSA',
		body: `Hola,

Usa el siguiente código de verificación para enviar tu reporte de la Ley de Servicios Digitales en Fluxer:

{code}

Este código expira el {expiresAt, date, full} {expiresAt, time, short}.

Si no solicitaste esto, por favor ignora este correo.

- Equipo de Seguridad de Fluxer`,
	},
	registrationApproved: {
		subject: 'Tu registro en Fluxer ha sido aprobado',
		body: `Hola {username},

¡Buenas noticias! Tu registro en Fluxer ha sido aprobado.

Ahora puedes iniciar sesión en la aplicación de Fluxer en:
{channelsUrl}

¡Bienvenido a la comunidad de Fluxer!

- Equipo de Fluxer`,
	},
	emailChangeRevert: {
		subject: 'Tu correo de Fluxer ha cambiado',
		body: `Hola {username},

El correo electrónico de tu cuenta de Fluxer se cambió a {newEmail}.

Si realizaste este cambio, no necesitas hacer nada. Si no, puedes revertirlo y proteger tu cuenta con este enlace:

{revertUrl}

Esto restaurará tu correo anterior, cerrará tu sesión en todos los dispositivos, eliminará los números de teléfono vinculados, desactivará el MFA y requerirá una nueva contraseña.

- Equipo de Seguridad de Fluxer`,
	},
};
