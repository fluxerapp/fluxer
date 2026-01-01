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

export const es419: EmailTranslations = {
	passwordReset: {
		subject: 'Restablece tu contraseña de Fluxer',
		body: `Hola {username},

Solicitaste restablecer la contraseña de tu cuenta de Fluxer. Por favor sigue el enlace de abajo para establecer una nueva contraseña:

{resetUrl}

Si no solicitaste este restablecimiento, puedes ignorar este correo.

Este enlace expirará en 1 hora.

- Equipo de Fluxer`,
	},
	emailVerification: {
		subject: 'Verifica tu correo electrónico de Fluxer',
		body: `Hola {username},

Por favor verifica tu correo electrónico para tu cuenta de Fluxer haciendo clic en el siguiente enlace:

{verifyUrl}

Si no creaste una cuenta en Fluxer, puedes ignorar este correo.

Este enlace expirará en 24 horas.

- Equipo de Fluxer`,
	},
	ipAuthorization: {
		subject: 'Autoriza el acceso desde una nueva dirección IP',
		body: `Hola {username},

Detectamos un intento de inicio de sesión en tu cuenta de Fluxer desde una nueva dirección IP:

Dirección IP: {ipAddress}
Ubicación: {location}

Si fuiste tú, autoriza esta dirección IP haciendo clic en el enlace:

{authUrl}

Si no intentaste iniciar sesión, cambia tu contraseña de inmediato.

Este enlace de autorización expirará en 30 minutos.

- Equipo de Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Tu cuenta de Fluxer fue deshabilitada temporalmente',
		body: `Hola {username},

Tu cuenta de Fluxer fue deshabilitada temporalmente debido a actividad sospechosa.

{reason, select,
	null {}
	other {Motivo: {reason}

}}Para recuperar acceso a tu cuenta, debes restablecer tu contraseña:

{forgotUrl}

Después de restablecerla, podrás iniciar sesión nuevamente.

Si crees que esto fue un error, contacta a soporte.

- Equipo de Seguridad de Fluxer`,
	},
	accountTempBanned: {
		subject: 'Tu cuenta de Fluxer fue suspendida temporalmente',
		body: `Hola {username},

Tu cuenta de Fluxer fue suspendida temporalmente por violar nuestros Términos de Servicio o Guías de la Comunidad.

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

Durante este tiempo no podrás acceder a tu cuenta.

Te recomendamos leer:
- Términos de Servicio: {termsUrl}
- Guías de la Comunidad: {guidelinesUrl}

Si crees que esta decisión es incorrecta o injustificada, puedes enviar una apelación a appeals@fluxer.app desde este correo. Explica claramente por qué consideras que la decisión fue equivocada. Revisaremos tu apelación y te responderemos con un resultado.

- Equipo de Seguridad de Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Tu cuenta de Fluxer está programada para eliminación',
		body: `Hola {username},

Tu cuenta de Fluxer fue programada para eliminación permanente debido a violaciones de nuestros Términos de Servicio o Guías de la Comunidad.

Fecha programada de eliminación: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Motivo: {reason}}
}

Esta es una acción seria. Los datos de tu cuenta serán eliminados permanentemente en esa fecha.

Te recomendamos revisar:
- Términos de Servicio: {termsUrl}
- Guías de la Comunidad: {guidelinesUrl}

PROCESO DE APELACIÓN:
Si crees que esta acción no es correcta, tienes 30 días para enviar una apelación a appeals@fluxer.app desde este correo.

En tu apelación debes:
- Explicar claramente por qué crees que la decisión es incorrecta
- Proporcionar evidencia o contexto relevante

Un miembro del Equipo de Seguridad revisará tu caso y puede pausar la eliminación hasta llegar a una determinación final.

- Equipo de Seguridad de Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'La eliminación de tu cuenta de Fluxer ha sido programada',
		body: `Hola {username},

¡Lamentamos que te vayas! Tu cuenta de Fluxer ha sido programada para eliminación.

Fecha programada de eliminación: {deletionDate, date, full} {deletionDate, time, short}

IMPORTANTE: Puedes cancelar la eliminación en cualquier momento antes de {deletionDate, date, full} {deletionDate, time, short} iniciando sesión nuevamente.

ANTES DE IRTE:
En el Panel de Privacidad en Configuración de Usuario puedes:
- Borrar tus mensajes
- Exportar datos importantes antes de salir

Nota: Cuando tu cuenta sea eliminada, ya no será posible borrar tus mensajes. Si deseas hacerlo, hazlo antes desde el Panel de Privacidad.

Si cambias de opinión, solo inicia sesión para cancelar la eliminación.

- Equipo de Fluxer`,
	},
	inactivityWarning: {
		subject: 'Tu cuenta de Fluxer será eliminada por inactividad',
		body: `Hola {username},

Notamos que no has iniciado sesión en tu cuenta de Fluxer por más de 2 años.

Último inicio de sesión: {lastActiveDate, date, full} {lastActiveDate, time, short}

De acuerdo con nuestra política de retención de datos, las cuentas inactivas se programan para eliminación. Tu cuenta será eliminada definitivamente en:

Fecha programada de eliminación: {deletionDate, date, full} {deletionDate, time, short}

CÓMO CONSERVAR TU CUENTA:
Solo debes iniciar sesión antes de la fecha indicada para cancelar esta eliminación automática. No necesitas hacer nada más.

SI NO INICIAS SESIÓN:
- Tu cuenta y todos tus datos serán eliminados permanentemente
- Tus mensajes serán anonimizados (“Usuario eliminado”)
- Esta acción no puede deshacerse

¿QUIERES BORRAR TUS MENSAJES?
Inicia sesión y usa el Panel de Privacidad antes de que tu cuenta sea eliminada.

¡Esperamos verte pronto de vuelta en Fluxer!

- Equipo de Fluxer`,
	},
	harvestCompleted: {
		subject: 'Tu exportación de datos de Fluxer está lista',
		body: `Hola {username},

¡Tu exportación de datos está lista para descargar!

Resumen:
- Mensajes totales: {totalMessages, number}
- Tamaño del archivo: {fileSizeMB} MB
- Formato: Archivo ZIP con archivos JSON

Descargar datos: {downloadUrl}

IMPORTANTE: El enlace expirará el {expiresAt, date, full} {expiresAt, time, short}

Incluye:
- Todos tus mensajes organizados por canal
- Metadatos de canales
- Información de cuenta y perfil
- Membresías y configuraciones
- Sesiones de autenticación e información de seguridad

Datos en formato JSON para fácil análisis.

Si tienes dudas, escribe a support@fluxer.app

- Equipo de Fluxer`,
	},
	unbanNotification: {
		subject: 'Tu suspensión de Fluxer ha sido levantada',
		body: `Hola {username},

¡Buenas noticias! Tu suspensión en Fluxer ha sido levantada.

Motivo: {reason}

Ahora puedes iniciar sesión nuevamente y continuar usando Fluxer.

- Equipo de Seguridad de Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Tu cuenta de Fluxer está programada para eliminación',
		body: `Hola {username},

Tu cuenta de Fluxer fue programada para eliminación definitiva.

Fecha programada de eliminación: {deletionDate, date, full} {deletionDate, time, short}
Motivo: {reason}

Esta es una acción seria. Tus datos serán eliminados permanentemente.

Puedes apelar escribiendo a appeals@fluxer.app desde este correo.

- Equipo de Seguridad de Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Tu regalo de Fluxer Premium fue revocado',
		body: `Hola {username},

Te informamos que el regalo de Fluxer Premium que canjeaste fue revocado debido a un contracargo (chargeback) realizado por el comprador original.

Los beneficios Premium fueron eliminados de tu cuenta. Esto ocurrió porque el pago fue revertido.

Si tienes preguntas, contáctanos en support@fluxer.app.

- Equipo de Fluxer`,
	},
	reportResolved: {
		subject: 'Tu reporte en Fluxer ha sido revisado',
		body: `Hola {username},

Tu reporte (ID: {reportId}) fue revisado por nuestro Equipo de Seguridad.

Respuesta del equipo:
{publicComment}

Gracias por ayudar a mantener Fluxer seguro para todos. Valoramos tu colaboración.

Si tienes dudas o inquietudes, escribe a safety@fluxer.app.

- Equipo de Seguridad de Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Verifica tu correo electrónico para un reporte DSA',
		body: `Hola,

Usa el siguiente código de verificación para enviar tu reporte de la Ley de Servicios Digitales en Fluxer:

{code}

Este código expira el {expiresAt, date, full} {expiresAt, time, short}.

Si no solicitaste esto, puedes ignorar este correo.

- Equipo de Seguridad de Fluxer`,
	},
	registrationApproved: {
		subject: 'Tu registro en Fluxer fue aprobado',
		body: `Hola {username},

¡Buenas noticias! Tu registro en Fluxer fue aprobado.

Ahora puedes ingresar a la aplicación en:
{channelsUrl}

¡Bienvenido a la comunidad de Fluxer!

- Equipo de Fluxer`,
	},
	emailChangeRevert: {
		subject: 'Tu correo de Fluxer fue cambiado',
		body: `Hola {username},

El correo electrónico de tu cuenta de Fluxer se cambió a {newEmail}.

Si hiciste este cambio, no necesitas hacer nada. Si no, puedes deshacerlo y proteger tu cuenta con este enlace:

{revertUrl}

Esto restaurará tu correo anterior, cerrará sesión en todos los dispositivos, eliminará los números de teléfono asociados, desactivará el MFA y requerirá una nueva contraseña.

- Equipo de Seguridad de Fluxer`,
	},
};
