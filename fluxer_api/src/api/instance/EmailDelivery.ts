// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {APIConfig} from '@app/api/config/APIConfig';
import {
	type EmailDeliveryMode,
	EmailDeliveryModes,
	type EmailDeliveryState,
	emailDeliveryFromLegacy,
} from '@fluxer/constants/src/AccountConfigConstants';
import type {AccountIdentityMode} from '@fluxer/constants/src/AccountIdentityConstants';
import {resolveEmailDeliveryState} from '@fluxer/constants/src/AccountTierRules';
import {type SmtpTlsMode, smtpTlsModeFromSecure} from '@fluxer/constants/src/SmtpConstants';

export type EffectiveSmtpConfig = NonNullable<APIConfig['email']['smtp']>;

export interface StoredEmailSmtpConfig {
	host: string | null;
	port: number | null;
	username: string | null;
	password: string | null;
	tls_mode: SmtpTlsMode | null;
	secure: boolean | null;
}

interface StoredEmailDeliveryConfig {
	delivery: EmailDeliveryMode | null;
	from_email: string | null;
	smtp: StoredEmailSmtpConfig;
}

const NO_STORED_EMAIL_DELIVERY: StoredEmailDeliveryConfig = {
	delivery: null,
	from_email: null,
	smtp: {host: null, port: null, username: null, password: null, tls_mode: null, secure: null},
};

function envEmailDelivery(): EmailDeliveryMode {
	return emailDeliveryFromLegacy({enabled: Config.email.enabled, provider: Config.email.provider});
}

export function legacyStoredEmailDelivery(
	enabled: boolean | null,
	provider: APIConfig['email']['provider'] | null,
): EmailDeliveryMode | null {
	if (enabled === null && provider === null) return null;
	return emailDeliveryFromLegacy({
		enabled: enabled ?? Config.email.enabled,
		provider: provider ?? Config.email.provider,
	});
}

export function resolveEmailDelivery(stored: Pick<StoredEmailDeliveryConfig, 'delivery'>): EmailDeliveryMode {
	return stored.delivery ?? envEmailDelivery();
}

export function storedSmtpTlsMode(smtp: StoredEmailSmtpConfig): SmtpTlsMode | null {
	return smtp.tls_mode ?? smtpTlsModeFromSecure(smtp.secure, smtp.port ?? Config.email.smtp?.port);
}

export function resolveEffectiveSmtpConfig(smtp: StoredEmailSmtpConfig): EffectiveSmtpConfig {
	return {
		host: smtp.host ?? Config.email.smtp?.host ?? '',
		port: smtp.port ?? Config.email.smtp?.port ?? 587,
		username: smtp.username ?? Config.email.smtp?.username ?? '',
		password: smtp.password ?? Config.email.smtp?.password ?? '',
		tlsMode: storedSmtpTlsMode(smtp) ?? Config.email.smtp?.tlsMode ?? null,
	};
}

export function describeIncompleteSmtpConfig(smtp: EffectiveSmtpConfig): string | null {
	if (!smtp.host.trim()) return 'No SMTP host is configured';
	if (!smtp.port) return 'No SMTP port is configured';
	if (!smtp.username.trim() !== !smtp.password.trim()) {
		return 'Set both the SMTP username and password, or leave both empty for a server that needs no sign-in';
	}
	return null;
}

export function resolveStoredEmailDeliveryState(
	identityMode: AccountIdentityMode,
	stored: StoredEmailDeliveryConfig,
): EmailDeliveryState {
	return resolveEmailDeliveryState({
		identityMode,
		delivery: resolveEmailDelivery(stored),
		smtpComplete: describeIncompleteSmtpConfig(resolveEffectiveSmtpConfig(stored.smtp)) === null,
		fromAddress: stored.from_email ?? Config.email.fromEmail,
	});
}

export function envEmailDeliveryState(identityMode: AccountIdentityMode): EmailDeliveryState {
	return resolveStoredEmailDeliveryState(identityMode, NO_STORED_EMAIL_DELIVERY);
}

export function emailProviderFor(delivery: EmailDeliveryMode): APIConfig['email']['provider'] {
	return delivery === EmailDeliveryModes.SMTP ? 'smtp' : 'none';
}
