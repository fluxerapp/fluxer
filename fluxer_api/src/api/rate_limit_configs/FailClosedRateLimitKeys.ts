// SPDX-License-Identifier: AGPL-3.0-or-later

const BUCKET_KEY_PREFIX = 'ratelimit:bucket:';
const IDENTIFIER_KEY_PREFIX = 'ratelimit:';

const FAIL_CLOSED_BUCKET_PREFIXES = [
	'auth:',
	'mfa:',
	'sudo:',
	'oauth:',
	'instance:setup:',
	'user:password',
	'user:email_change:',
	'user:email_verification',
	'user:mfa:',
	'user:recovery_kit:',
	'user:authorized_ips:',
	'user:app_attest:',
	'dsa:report:email:',
	'donation:request_link:email',
	'donation:checkout:email',
] as const;

const FAIL_CLOSED_IDENTIFIER_PREFIXES = [
	'login:',
	'mfa:',
	'sudo-mfa:',
	'mfa_backup_codes_challenge:',
	'password_reset:',
	'password_change:',
	'email_change:',
	'email_verification:',
	'email_send:',
	'registration:',
	'recover:',
	'setup-token:',
	'lookup:',
] as const;

export function isFailClosedRateLimitKey(key: string): boolean {
	if (key.startsWith(BUCKET_KEY_PREFIX)) {
		const scoped = key.slice(BUCKET_KEY_PREFIX.length);
		return FAIL_CLOSED_BUCKET_PREFIXES.some((prefix) => scoped.includes(`:${prefix}`));
	}
	if (key.startsWith('ratelimit:global:')) return false;
	if (!key.startsWith(IDENTIFIER_KEY_PREFIX)) return false;
	const identifier = key.slice(IDENTIFIER_KEY_PREFIX.length);
	return FAIL_CLOSED_IDENTIFIER_PREFIXES.some((prefix) => identifier.startsWith(prefix));
}
