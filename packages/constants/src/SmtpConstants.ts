// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const SmtpTlsModes = {
	IMPLICIT: 'implicit',
	STARTTLS: 'starttls',
	OPPORTUNISTIC: 'opportunistic',
	NONE: 'none',
} as const;

export type SmtpTlsMode = ValueOf<typeof SmtpTlsModes>;

const SMTP_IMPLICIT_TLS_PORT = 465;

export function resolveSmtpTlsMode(mode: SmtpTlsMode | null | undefined, port: number): SmtpTlsMode {
	if (mode) return mode;
	return port === SMTP_IMPLICIT_TLS_PORT ? SmtpTlsModes.IMPLICIT : SmtpTlsModes.STARTTLS;
}

export function smtpTlsModeFromSecure(secure: boolean | null | undefined): SmtpTlsMode | null {
	if (secure == null) return null;
	return secure ? SmtpTlsModes.IMPLICIT : SmtpTlsModes.OPPORTUNISTIC;
}
