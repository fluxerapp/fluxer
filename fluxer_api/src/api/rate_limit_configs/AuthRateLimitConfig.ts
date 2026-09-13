// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const AuthRateLimitConfigs = {
	AUTH_REGISTER: {
		bucket: 'auth:register',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	AUTH_LOGIN: {
		bucket: 'auth:login',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	AUTH_SSO_START: {
		bucket: 'auth:sso:start',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	AUTH_SSO_COMPLETE: {
		bucket: 'auth:sso:complete',
		config: {limit: 15, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	AUTH_LOGIN_MFA: {
		bucket: 'auth:login:mfa',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_VERIFY_EMAIL: {
		bucket: 'auth:verify',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_RESEND_VERIFICATION: {
		bucket: 'auth:verify:resend',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_FORGOT_PASSWORD: {
		bucket: 'auth:forgot',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_RESET_PASSWORD: {
		bucket: 'auth:reset',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_VALIDATE_RESET_TOKEN: {
		bucket: 'auth:reset:validate',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_EMAIL_REVERT: {
		bucket: 'auth:email_revert',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_SESSIONS_GET: {
		bucket: 'auth:sessions',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	AUTH_SESSIONS_LOGOUT: {
		bucket: 'auth:sessions:logout',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	AUTH_AUTHORIZE_IP: {
		bucket: 'auth:authorize_ip',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_IP_AUTHORIZATION_RESEND: {
		bucket: 'auth:ip_authorization_resend',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_IP_AUTHORIZATION_POLL: {
		bucket: 'auth:ip_authorization_poll',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_LOGOUT: {
		bucket: 'auth:logout',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	AUTH_WEBAUTHN_OPTIONS: {
		bucket: 'auth:webauthn:options',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	AUTH_WEBAUTHN_AUTHENTICATE: {
		bucket: 'auth:webauthn:authenticate',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	MFA_WEBAUTHN_LIST: {
		bucket: 'mfa:webauthn:list',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	MFA_WEBAUTHN_REGISTRATION_OPTIONS: {
		bucket: 'mfa:webauthn:registration_options',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	MFA_WEBAUTHN_REGISTER: {
		bucket: 'mfa:webauthn:register',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	MFA_WEBAUTHN_UPDATE: {
		bucket: 'mfa:webauthn:update',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	MFA_WEBAUTHN_DELETE: {
		bucket: 'mfa:webauthn:delete',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	PHONE_SEND_VERIFICATION: {
		bucket: 'phone:send_verification',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	PHONE_VERIFY_CODE: {
		bucket: 'phone:verify_code',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_HANDOFF_INITIATE: {
		bucket: 'auth:handoff:initiate',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_HANDOFF_INFO: {
		bucket: 'auth:handoff:info',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_HANDOFF_COMPLETE: {
		bucket: 'auth:handoff:complete',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_HANDOFF_STATUS: {
		bucket: 'auth:handoff:status',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AUTH_HANDOFF_CANCEL: {
		bucket: 'auth:handoff:cancel',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	SUDO_WEBAUTHN_OPTIONS: {
		bucket: 'sudo:webauthn:options',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	SUDO_MFA_METHODS: {
		bucket: 'sudo:mfa:methods',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
} as const;
