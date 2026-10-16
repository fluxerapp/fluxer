// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {GiftAcceptModal} from '@app/features/expressions/components/modals/GiftAcceptModal';
import {GiftSendToFriendModal} from '@app/features/expressions/components/modals/GiftSendToFriendModal';
import Gifts from '@app/features/gift/state/Gifts';
import {
	FAILED_TO_REDEEM_GIFT_DESCRIPTOR,
	GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR,
	GIFT_NOT_FOUND_TITLE_DESCRIPTOR,
} from '@app/features/gift/utils/GiftMessageDescriptors';
import {extractGiftCode} from '@app/features/gift/utils/GiftUtils';
import {type InstanceHTTPTarget, instanceRequest} from '@app/features/platform/transport/InstanceHTTP';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const GIFT_REDEEMED_SUCCESSFULLY_DESCRIPTOR = msg({
	message: 'Gift redeemed!',
	comment: 'Success toast after a gift code is successfully redeemed.',
});
const INVALID_GIFT_CODE_DESCRIPTOR = msg({
	comment: 'Gift redemption error modal title for an invalid or already used code.',
	message: 'Invalid gift code',
});
const THIS_GIFT_CODE_IS_INVALID_OR_HAS_ALREADY_BEEN_REDEEMED_DESCRIPTOR = msg({
	comment: 'Gift redemption error modal body for an invalid or already used code.',
	message: 'This code is invalid or already used.',
});
const THIS_GIFT_CODE_HAS_ALREADY_BEEN_REDEEMED_DESCRIPTOR = msg({
	comment: 'Gift redemption error modal body for a code that was already redeemed.',
	message: 'This code was already redeemed.',
});
const THIS_GIFT_CODE_COULD_NOT_BE_FOUND_DESCRIPTOR = msg({
	comment: 'Gift redemption error modal body for a code that does not exist.',
	message: "This code doesn't exist.",
});
const WE_COULDN_T_REDEEM_THIS_GIFT_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	comment: 'Generic gift redemption error modal body. Keep calm and concise.',
	message: "Couldn't redeem this gift. Try again.",
});

interface ApiErrorResponse {
	code?: string;
	message?: string;
	errors?: Record<string, unknown>;
}

const logger = new Logger('Gifts');
const MOCK_USER_PARTIAL: UserPartial = {
	id: '000000000000000000',
	username: 'MockUser',
	discriminator: '0000',
	global_name: null,
	avatar: null,
	avatar_color: null,
	flags: 0,
};

export type GiftDurationType = 'days' | 'weeks' | 'months' | 'years';

function mapLegacyDurationMonthsToFields(durationMonths: number): {
	duration_type: GiftDurationType;
	duration_quantity: number;
} {
	if (durationMonths !== 0 && durationMonths % 12 === 0) {
		return {
			duration_type: 'years',
			duration_quantity: durationMonths / 12,
		};
	}
	return {
		duration_type: 'months',
		duration_quantity: durationMonths,
	};
}

function currentUserPartial(): UserPartial {
	const currentUser = Users.getCurrentUser();
	return currentUser
		? {
				id: currentUser.id,
				username: currentUser.username,
				discriminator: currentUser.discriminator,
				global_name: currentUser.globalName,
				avatar: currentUser.avatar,
				avatar_color: currentUser.avatarColor ?? null,
				flags: currentUser.flags,
			}
		: MOCK_USER_PARTIAL;
}

function errorCode(error: HttpError): string | undefined {
	return (error.body as ApiErrorResponse | undefined)?.code;
}

function giftErrorModal(title: string, message: string): void {
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={title}
				message={message}
				data-flx="gift.gift-commands.gift-error-modal.generic-error-modal"
			/>
		)),
	);
}

function handleRedeemFailure(i18n: I18n, code: string, target: InstanceHTTPTarget, error: HttpError): void {
	switch (errorCode(error)) {
		case APIErrorCodes.CANNOT_REDEEM_PLUTONIUM_WITH_VISIONARY:
			ModalCommands.push(
				modal(() => (
					<GiftSendToFriendModal
						code={code}
						data-flx="gift.gift-commands.handle-redeem-failure.gift-send-to-friend-modal"
					/>
				)),
			);
			break;
		case APIErrorCodes.UNKNOWN_GIFT_CODE:
			Gifts.markAsInvalid(code, target);
			giftErrorModal(
				i18n._(INVALID_GIFT_CODE_DESCRIPTOR),
				i18n._(THIS_GIFT_CODE_IS_INVALID_OR_HAS_ALREADY_BEEN_REDEEMED_DESCRIPTOR),
			);
			break;
		case APIErrorCodes.GIFT_CODE_ALREADY_REDEEMED:
			Gifts.markAsRedeemed(code, target);
			giftErrorModal(
				i18n._(GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR),
				i18n._(THIS_GIFT_CODE_HAS_ALREADY_BEEN_REDEEMED_DESCRIPTOR),
			);
			break;
		default:
			if (error.status === 404) {
				Gifts.markAsInvalid(code, target);
				giftErrorModal(i18n._(GIFT_NOT_FOUND_TITLE_DESCRIPTOR), i18n._(THIS_GIFT_CODE_COULD_NOT_BE_FOUND_DESCRIPTOR));
			} else {
				giftErrorModal(
					i18n._(FAILED_TO_REDEEM_GIFT_DESCRIPTOR),
					i18n._(WE_COULDN_T_REDEEM_THIS_GIFT_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR),
				);
			}
	}
}

function fallbackRedeemFailure(i18n: I18n): void {
	giftErrorModal(
		i18n._(FAILED_TO_REDEEM_GIFT_DESCRIPTOR),
		i18n._(WE_COULDN_T_REDEEM_THIS_GIFT_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR),
	);
}

function mockGiftMetadata(): Array<GiftMetadata> {
	const userPartial = currentUserPartial();
	const now = new Date();
	const sevenDaysAgo = new Date(now.getTime() - 7 * MS_PER_DAY);
	const twoDaysAgo = new Date(now.getTime() - 2 * MS_PER_DAY);
	const durationMonths = DeveloperOptions.mockGiftDurationMonths ?? 12;
	const isRedeemed = DeveloperOptions.mockGiftRedeemed ?? false;
	const duration = mapLegacyDurationMonthsToFields(durationMonths);
	return [
		{
			code: 'MOCK-GIFT-TEST-1234',
			duration_type: duration.duration_type,
			duration_quantity: duration.duration_quantity,
			created_at: sevenDaysAgo.toISOString(),
			created_by: userPartial,
			redeemed_at: isRedeemed ? twoDaysAgo.toISOString() : null,
			redeemed_by: isRedeemed ? userPartial : null,
		},
	];
}

export interface Gift {
	code: string;
	duration_type: GiftDurationType;
	duration_quantity: number;
	redeemed: boolean;
	created_by?: UserPartial;
}

export interface GiftMetadata {
	code: string;
	duration_type: GiftDurationType;
	duration_quantity: number;
	created_at: string;
	created_by: UserPartial;
	redeemed_at: string | null;
	redeemed_by: UserPartial | null;
}

export async function fetch(rawCode: string, target: InstanceHTTPTarget): Promise<Gift> {
	const code = extractGiftCode(rawCode);
	try {
		const response = await instanceRequest<Gift>({method: 'GET', path: Endpoints.GIFT(code), target});
		const gift = response.body;
		logger.debug('Gift fetched', {code});
		return gift;
	} catch (error) {
		logger.error('Gift fetch failed', error);
		throw error;
	}
}

export async function fetchWithCoalescing(rawCode: string, target: InstanceHTTPTarget): Promise<Gift> {
	return Gifts.fetchGift(extractGiftCode(rawCode), target);
}

export async function openAcceptModal(rawCode: string, target: InstanceHTTPTarget): Promise<void> {
	const code = extractGiftCode(rawCode);
	void fetchWithCoalescing(code, target).catch(() => {});
	ModalCommands.pushWithKey(
		modal(() => (
			<GiftAcceptModal code={code} target={target} data-flx="gift.gift-commands.open-accept-modal.gift-accept-modal" />
		)),
		`gift-accept-${code}`,
	);
}

export async function redeem(i18n: I18n, rawCode: string, target: InstanceHTTPTarget): Promise<void> {
	const code = extractGiftCode(rawCode);
	try {
		await instanceRequest({method: 'POST', path: Endpoints.GIFT_REDEEM(code), target});
		logger.info('Gift redeemed', {code});
		Gifts.markAsRedeemed(code, target);
		ToastCommands.success(i18n._(GIFT_REDEEMED_SUCCESSFULLY_DESCRIPTOR));
	} catch (error) {
		logger.error('Gift redeem failed', error);
		if (error instanceof HttpError) {
			handleRedeemFailure(i18n, code, target, error);
		} else {
			fallbackRedeemFailure(i18n);
		}
		throw error;
	}
}

export async function fetchUserGifts(): Promise<Array<GiftMetadata>> {
	if (DeveloperOptions.mockGiftInventory) {
		logger.debug('Returning mock user gifts', {count: 1});
		return mockGiftMetadata();
	}
	try {
		const response = await http.get<Array<GiftMetadata>>(Endpoints.USER_GIFTS);
		const gifts = response.body;
		logger.debug('User gifts fetched', {count: gifts.length});
		return gifts;
	} catch (error) {
		logger.error('User gifts fetch failed', error);
		throw error;
	}
}
