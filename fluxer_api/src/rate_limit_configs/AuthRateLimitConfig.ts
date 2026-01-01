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

import type {RouteRateLimitConfig} from '~/middleware/RateLimitMiddleware';

export const AuthRateLimitConfigs = {
	AUTH_REGISTER: {
		bucket: 'auth:register',
		config: {limit: 10, windowMs: 10000},
	} as RouteRateLimitConfig,

	AUTH_LOGIN: {
		bucket: 'auth:login',
		config: {limit: 10, windowMs: 10000},
	} as RouteRateLimitConfig,

	AUTH_LOGIN_MFA: {
		bucket: 'auth:login:mfa',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	AUTH_VERIFY_EMAIL: {
		bucket: 'auth:verify',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_RESEND_VERIFICATION: {
		bucket: 'auth:verify:resend',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_FORGOT_PASSWORD: {
		bucket: 'auth:forgot',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_RESET_PASSWORD: {
		bucket: 'auth:reset',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_EMAIL_REVERT: {
		bucket: 'auth:email_revert',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_SESSIONS_GET: {
		bucket: 'auth:sessions',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	AUTH_SESSIONS_LOGOUT: {
		bucket: 'auth:sessions:logout',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	AUTH_AUTHORIZE_IP: {
		bucket: 'auth:authorize_ip',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_IP_AUTHORIZATION_RESEND: {
		bucket: 'auth:ip_authorization_resend',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_IP_AUTHORIZATION_STREAM: {
		bucket: 'auth:ip_authorization_stream',
		config: {limit: 30, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_LOGOUT: {
		bucket: 'auth:logout',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	AUTH_WEBAUTHN_OPTIONS: {
		bucket: 'auth:webauthn:options',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	AUTH_WEBAUTHN_AUTHENTICATE: {
		bucket: 'auth:webauthn:authenticate',
		config: {limit: 10, windowMs: 10000},
	} as RouteRateLimitConfig,

	MFA_SMS_ENABLE: {
		bucket: 'mfa:sms:enable',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	MFA_SMS_DISABLE: {
		bucket: 'mfa:sms:disable',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	MFA_WEBAUTHN_LIST: {
		bucket: 'mfa:webauthn:list',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	MFA_WEBAUTHN_REGISTRATION_OPTIONS: {
		bucket: 'mfa:webauthn:registration_options',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	MFA_WEBAUTHN_REGISTER: {
		bucket: 'mfa:webauthn:register',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	MFA_WEBAUTHN_UPDATE: {
		bucket: 'mfa:webauthn:update',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	MFA_WEBAUTHN_DELETE: {
		bucket: 'mfa:webauthn:delete',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	PHONE_SEND_VERIFICATION: {
		bucket: 'phone:send_verification',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	PHONE_VERIFY_CODE: {
		bucket: 'phone:verify_code',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	PHONE_ADD: {
		bucket: 'phone:add',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	PHONE_REMOVE: {
		bucket: 'phone:remove',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_HANDOFF_INITIATE: {
		bucket: 'auth:handoff:initiate',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_HANDOFF_COMPLETE: {
		bucket: 'auth:handoff:complete',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_HANDOFF_STATUS: {
		bucket: 'auth:handoff:status',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	AUTH_HANDOFF_CANCEL: {
		bucket: 'auth:handoff:cancel',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	SUDO_SMS_SEND: {
		bucket: 'sudo:sms:send',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	SUDO_WEBAUTHN_OPTIONS: {
		bucket: 'sudo:webauthn:options',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	SUDO_MFA_METHODS: {
		bucket: 'sudo:mfa:methods',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,
} as const;
