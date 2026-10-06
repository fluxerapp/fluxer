// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const PAYMENT_UNAVAILABLE_TITLE_DESCRIPTOR = msg({
	message: 'Payments are unavailable',
	comment: 'Title of the error modal shown when the payment provider is not configured or reachable.',
});
const PAYMENT_UNAVAILABLE_MESSAGE_DESCRIPTOR = msg({
	message: 'Payments are temporarily unavailable. Please try again in a moment.',
	comment: 'Body of the error modal shown when the payment provider is not configured or reachable.',
});
const NO_SUBSCRIPTION_TITLE_DESCRIPTOR = msg({
	message: 'No active subscription',
	comment: 'Title of the error modal shown when an action needs an active subscription but none exists.',
});
const NO_SUBSCRIPTION_MESSAGE_DESCRIPTOR = msg({
	message: "We couldn't find an active subscription on this account. Refresh and try again.",
	comment: 'Body of the error modal shown when an action needs an active subscription but none exists.',
});
const ALREADY_CANCELING_TITLE_DESCRIPTOR = msg({
	message: 'Already set to cancel',
	comment: 'Title of the error modal shown when a subscription is already scheduled to cancel.',
});
const ALREADY_CANCELING_MESSAGE_DESCRIPTOR = msg({
	message: 'Your subscription is already set to cancel at the end of your billing period.',
	comment: 'Body of the error modal shown when a subscription is already scheduled to cancel.',
});
const NOT_CANCELING_TITLE_DESCRIPTOR = msg({
	message: 'Subscription is not scheduled to cancel',
	comment: 'Title of the error modal shown when reactivation is attempted but the subscription is not cancelling.',
});
const NOT_CANCELING_MESSAGE_DESCRIPTOR = msg({
	message: "Your subscription isn't set to cancel, so there's nothing to reactivate.",
	comment: 'Body of the error modal shown when reactivation is attempted but the subscription is not cancelling.',
});

interface PremiumActionErrorCopy {
	fallbackTitle: MessageDescriptor;
	fallbackMessage: MessageDescriptor;
}

function resolvePremiumActionErrorContent(
	code: string | undefined,
	copy: PremiumActionErrorCopy,
): {title: string; message: string} {
	switch (code) {
		case APIErrorCodes.STRIPE_PAYMENT_NOT_AVAILABLE:
			return {
				title: i18n._(PAYMENT_UNAVAILABLE_TITLE_DESCRIPTOR),
				message: i18n._(PAYMENT_UNAVAILABLE_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.STRIPE_NO_ACTIVE_SUBSCRIPTION:
		case APIErrorCodes.STRIPE_NO_SUBSCRIPTION:
			return {
				title: i18n._(NO_SUBSCRIPTION_TITLE_DESCRIPTOR),
				message: i18n._(NO_SUBSCRIPTION_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.STRIPE_SUBSCRIPTION_ALREADY_CANCELING:
			return {
				title: i18n._(ALREADY_CANCELING_TITLE_DESCRIPTOR),
				message: i18n._(ALREADY_CANCELING_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.STRIPE_SUBSCRIPTION_NOT_CANCELING:
			return {
				title: i18n._(NOT_CANCELING_TITLE_DESCRIPTOR),
				message: i18n._(NOT_CANCELING_MESSAGE_DESCRIPTOR),
			};
		default:
			return {
				title: i18n._(copy.fallbackTitle),
				message: i18n._(copy.fallbackMessage),
			};
	}
}

export function showPremiumActionErrorModal(error: unknown, copy: PremiumActionErrorCopy, flxKey: string): void {
	const code = failureCode(error);
	ModalCommands.push(
		modal(() => {
			const {title, message} = resolvePremiumActionErrorContent(code, copy);
			return <GenericErrorModal title={title} message={message} data-flx={flxKey} />;
		}),
	);
}
