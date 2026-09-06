// SPDX-License-Identifier: AGPL-3.0-or-later

import {SUPPORT_EMAIL} from '@app/features/app/config/I18nDisplayConstants';
import {
	ESCAPE_CONFIRM_EMAIL_REMAINS_DESCRIPTOR,
	ESCAPE_CONFIRM_LEAVE_DESCRIPTOR,
	ESCAPE_CONFIRM_OUTCOME_DESCRIPTOR,
	ESCAPE_CONFIRM_OWNED_DESCRIPTOR,
	ESCAPE_CONFIRM_PRIMARY_NO_GUILDS_DESCRIPTOR,
	ESCAPE_CONFIRM_PRIMARY_WITH_GUILDS_DESCRIPTOR,
	ESCAPE_CONFIRM_SUPPORT_DESCRIPTOR,
	ESCAPE_CONFIRM_TITLE_NO_GUILDS_DESCRIPTOR,
	ESCAPE_CONFIRM_TITLE_WITH_GUILDS_DESCRIPTOR,
	ESCAPE_HINT_NO_GUILDS_DESCRIPTOR,
	ESCAPE_HINT_WITH_GUILDS_DESCRIPTOR,
} from '@app/features/auth/components/modals/required_action/RequiredActionDescriptors';
import type {I18n} from '@lingui/core';

export interface PhoneGateEscapePlan {
	guildNames: ReadonlyArray<string>;
	ownedGuildNames: ReadonlyArray<string>;
	emailStepRemains: boolean;
}

export interface PhoneGateEscapeConfirmCopy {
	title: string;
	bodyLines: ReadonlyArray<string>;
	primaryText: string;
	primaryVariant: 'primary' | 'danger';
}

export function buildPhoneGateEscapeHint(i18n: I18n, guildCount: number): string {
	return guildCount === 0
		? i18n._(ESCAPE_HINT_NO_GUILDS_DESCRIPTOR)
		: i18n._(ESCAPE_HINT_WITH_GUILDS_DESCRIPTOR, {count: guildCount});
}

export function buildPhoneGateEscapeConfirmCopy(i18n: I18n, plan: PhoneGateEscapePlan): PhoneGateEscapeConfirmCopy {
	const leaving = plan.guildNames.length > 0;
	const bodyLines: Array<string> = [i18n._(ESCAPE_CONFIRM_OUTCOME_DESCRIPTOR)];
	if (leaving) {
		bodyLines.push(i18n._(ESCAPE_CONFIRM_LEAVE_DESCRIPTOR, {guildNames: plan.guildNames.join(', ')}));
	}
	if (plan.ownedGuildNames.length > 0) {
		bodyLines.push(i18n._(ESCAPE_CONFIRM_OWNED_DESCRIPTOR, {ownedNames: plan.ownedGuildNames.join(', ')}));
	}
	if (plan.emailStepRemains) {
		bodyLines.push(i18n._(ESCAPE_CONFIRM_EMAIL_REMAINS_DESCRIPTOR));
	}
	bodyLines.push(i18n._(ESCAPE_CONFIRM_SUPPORT_DESCRIPTOR, {supportEmail: SUPPORT_EMAIL}));
	return {
		title: leaving
			? i18n._(ESCAPE_CONFIRM_TITLE_WITH_GUILDS_DESCRIPTOR, {count: plan.guildNames.length})
			: i18n._(ESCAPE_CONFIRM_TITLE_NO_GUILDS_DESCRIPTOR),
		bodyLines,
		primaryText: leaving
			? i18n._(ESCAPE_CONFIRM_PRIMARY_WITH_GUILDS_DESCRIPTOR)
			: i18n._(ESCAPE_CONFIRM_PRIMARY_NO_GUILDS_DESCRIPTOR),
		primaryVariant: leaving ? 'danger' : 'primary',
	};
}
