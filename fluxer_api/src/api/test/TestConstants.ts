// SPDX-License-Identifier: AGPL-3.0-or-later

export const TEST_TIMEOUTS = {
	IMMEDIATE: 20,
	QUICK: 100,
	DEFAULT: 1000,
	MEDIUM: 2000,
	LONG: 5000,
	TICKET_EXPIRY_GRACE: 2000,
	COOLDOWN_WAIT: 31000,
	HARVEST_EXPIRY_BOUNDARY: 7000,
	MAX: 10000,
} as const;
export const TEST_CREDENTIALS = {
	STRONG_PASSWORD: 'a-strong-password',
	ALT_PASSWORD_1: 'AnotherStrongPassword123!',
	ALT_PASSWORD_2: 'SecurePass-2024!',
	WEAK_PASSWORD: 'weak',
	EMPTY_PASSWORD: '',
} as const;
export const TEST_USER_DATA = {
	DEFAULT_DATE_OF_BIRTH: '2000-01-01',
	DEFAULT_GLOBAL_NAME: 'Test User',
	REGISTER_GLOBAL_NAME: 'Register User',
	LOGIN_GLOBAL_NAME: 'Login Test User',
	EMAIL_DOMAIN: 'example.com',
	USERNAME_PREFIX: 'itest',
	EMAIL_PREFIX: 'integration',
} as const;
export const HTTP_STATUS = {
	OK: 200,
	CREATED: 201,
	NO_CONTENT: 204,
	BAD_REQUEST: 400,
	UNAUTHORIZED: 401,
	FORBIDDEN: 403,
	NOT_FOUND: 404,
	CONFLICT: 409,
	INTERNAL_SERVER_ERROR: 500,
	SERVICE_UNAVAILABLE: 503,
	ACCEPTED: 202,
} as const;
export const TEST_IDS = {
	NONEXISTENT_GUILD: '999999999999999999',
	NONEXISTENT_CHANNEL: '999999999999999999',
	NONEXISTENT_USER: '999999999999999999',
	NONEXISTENT_MESSAGE: '999999999999999999',
	NONEXISTENT_WEBHOOK: '999999999999999999',
} as const;
export function generateUniquePassword(): string {
	return `SecurePass-${Date.now()}!`;
}

export function wait(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}
