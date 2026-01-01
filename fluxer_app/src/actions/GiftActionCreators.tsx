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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {APIErrorCodes} from '~/Constants';
import {GenericErrorModal} from '~/components/alerts/GenericErrorModal';
import {GiftAcceptModal} from '~/components/modals/GiftAcceptModal';
import {Endpoints} from '~/Endpoints';
import http, {HttpError} from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {UserPartial} from '~/records/UserRecord';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import GiftStore from '~/stores/GiftStore';
import UserStore from '~/stores/UserStore';

interface ApiErrorResponse {
	code?: string;
	message?: string;
	errors?: Record<string, unknown>;
}

const logger = new Logger('Gifts');

export interface Gift {
	code: string;
	duration_months: number;
	redeemed: boolean;
	created_by?: UserPartial;
}

export interface GiftMetadata {
	code: string;
	duration_months: number;
	created_at: string;
	created_by: UserPartial;
	redeemed_at: string | null;
	redeemed_by: UserPartial | null;
}

export const fetch = async (code: string): Promise<Gift> => {
	try {
		const response = await http.get<Gift>({url: Endpoints.GIFT(code)});
		const gift = response.body;
		logger.debug('Gift fetched', {code});
		return gift;
	} catch (error) {
		logger.error('Gift fetch failed', error);

		if (error instanceof HttpError && error.status === 404) {
			GiftStore.markAsInvalid(code);
		}

		throw error;
	}
};

export const fetchWithCoalescing = async (code: string): Promise<Gift> => {
	return GiftStore.fetchGift(code);
};

export const openAcceptModal = async (code: string): Promise<void> => {
	void fetchWithCoalescing(code).catch(() => {});
	ModalActionCreators.pushWithKey(
		modal(() => <GiftAcceptModal code={code} />),
		`gift-accept-${code}`,
	);
};

export const redeem = async (i18n: I18n, code: string): Promise<void> => {
	try {
		await http.post({url: Endpoints.GIFT_REDEEM(code)});
		logger.info('Gift redeemed', {code});
		GiftStore.markAsRedeemed(code);
		ToastActionCreators.success(i18n._(msg`Gift redeemed successfully!`));
	} catch (error) {
		logger.error('Gift redeem failed', error);

		if (error instanceof HttpError) {
			const errorResponse = error.body as ApiErrorResponse;
			const errorCode = errorResponse?.code;

			switch (errorCode) {
				case APIErrorCodes.CANNOT_REDEEM_PLUTONIUM_WITH_VISIONARY:
					ModalActionCreators.push(
						modal(() => (
							<GenericErrorModal
								title={i18n._(msg`Cannot Redeem Gift`)}
								message={i18n._(msg`You cannot redeem Plutonium gift codes while you have Visionary premium.`)}
							/>
						)),
					);
					break;
				case APIErrorCodes.UNKNOWN_GIFT_CODE:
					GiftStore.markAsInvalid(code);
					ModalActionCreators.push(
						modal(() => (
							<GenericErrorModal
								title={i18n._(msg`Invalid Gift Code`)}
								message={i18n._(msg`This gift code is invalid or has already been redeemed.`)}
							/>
						)),
					);
					break;
				case APIErrorCodes.GIFT_CODE_ALREADY_REDEEMED:
					GiftStore.markAsRedeemed(code);
					ModalActionCreators.push(
						modal(() => (
							<GenericErrorModal
								title={i18n._(msg`Gift Already Redeemed`)}
								message={i18n._(msg`This gift code has already been redeemed.`)}
							/>
						)),
					);
					break;
				default:
					if (error.status === 404) {
						GiftStore.markAsInvalid(code);
						ModalActionCreators.push(
							modal(() => (
								<GenericErrorModal
									title={i18n._(msg`Gift Not Found`)}
									message={i18n._(msg`This gift code could not be found.`)}
								/>
							)),
						);
					} else {
						ModalActionCreators.push(
							modal(() => (
								<GenericErrorModal
									title={i18n._(msg`Failed to Redeem Gift`)}
									message={i18n._(msg`We couldn't redeem this gift code. Please try again.`)}
								/>
							)),
						);
					}
			}
		} else {
			ModalActionCreators.push(
				modal(() => (
					<GenericErrorModal
						title={i18n._(msg`Failed to Redeem Gift`)}
						message={i18n._(msg`We couldn't redeem this gift code. Please try again.`)}
					/>
				)),
			);
		}

		throw error;
	}
};

export const fetchUserGifts = async (): Promise<Array<GiftMetadata>> => {
	if (DeveloperOptionsStore.mockGiftInventory) {
		const currentUser = UserStore.getCurrentUser();
		const userPartial: UserPartial = currentUser
			? {
					id: currentUser.id,
					username: currentUser.username,
					discriminator: currentUser.discriminator,
					avatar: currentUser.avatar,
					flags: currentUser.flags,
				}
			: {
					id: '000000000000000000',
					username: 'MockUser',
					discriminator: '0000',
					avatar: null,
					flags: 0,
				};

		const now = new Date();
		const sevenDaysAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
		const twoDaysAgo = new Date(now.getTime() - 2 * 24 * 60 * 60 * 1000);

		const durationMonths = DeveloperOptionsStore.mockGiftDurationMonths ?? 12;
		const isRedeemed = DeveloperOptionsStore.mockGiftRedeemed ?? false;

		const mockGift: GiftMetadata = {
			code: 'MOCK-GIFT-TEST-1234',
			duration_months: durationMonths,
			created_at: sevenDaysAgo.toISOString(),
			created_by: userPartial,
			redeemed_at: isRedeemed ? twoDaysAgo.toISOString() : null,
			redeemed_by: isRedeemed ? userPartial : null,
		};

		logger.debug('Returning mock user gifts', {count: 1});
		return [mockGift];
	}

	try {
		const response = await http.get<Array<GiftMetadata>>({url: Endpoints.USER_GIFTS});
		const gifts = response.body;
		logger.debug('User gifts fetched', {count: gifts.length});
		return gifts;
	} catch (error) {
		logger.error('User gifts fetch failed', error);
		throw error;
	}
};
