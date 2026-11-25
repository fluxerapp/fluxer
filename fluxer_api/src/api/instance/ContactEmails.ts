// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';

export interface ContactEmails {
	appealsEmail: string | null;
	privacyEmail: string | null;
	safetyEmail: string | null;
	supportEmail: string | null;
}

const HOSTED_CONTACT_EMAILS: ContactEmails = {
	appealsEmail: 'appeals@fluxer.com',
	privacyEmail: 'privacy@fluxer.com',
	safetyEmail: 'safety@fluxer.com',
	supportEmail: 'support@fluxer.com',
};

const SELF_HOSTED_CONTACT_EMAILS: ContactEmails = {
	appealsEmail: null,
	privacyEmail: null,
	safetyEmail: null,
	supportEmail: null,
};

const HOSTED_LEGAL_ADDRESS =
	'Fluxer Platform AB (reg. no. 559537-3993), a Swedish company with its registered seat in Haninge. Office at Östgötagatan 12, 116 25 Stockholm, Sweden. Post to Fluxer Platform AB, c/o Embassy House, at that address.';

export function resolveLegalAddress(): string | null {
	return Config.instance.selfHosted ? null : HOSTED_LEGAL_ADDRESS;
}

export function resolveContactEmails(): ContactEmails {
	return Config.instance.selfHosted ? SELF_HOSTED_CONTACT_EMAILS : HOSTED_CONTACT_EMAILS;
}
