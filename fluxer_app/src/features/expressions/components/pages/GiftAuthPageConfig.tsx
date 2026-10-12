// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {GiftHeader} from '@app/features/auth/flow/GiftHeader';
import type {ResourceAuthPageConfig} from '@app/features/auth/flow/ResourceAuthPages';
import type {Gift} from '@app/features/gift/commands/GiftCommands';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import Gifts from '@app/features/gift/state/Gifts';
import {
	GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR,
	GIFT_NOT_FOUND_TITLE_DESCRIPTOR,
} from '@app/features/gift/utils/GiftMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {GiftIcon} from '@phosphor-icons/react';
import {useEffect, useMemo} from 'react';

const CLAIM_GIFT_DESCRIPTOR = msg({message: 'Claim gift', comment: 'Action label on the gift redemption flow.'});
function RedeemedGiftState({prefix}: {prefix: string}) {
	const {i18n} = useLingui();
	return (
		<AuthErrorState
			icon={GiftIcon}
			title={i18n._(GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR)}
			text={<Trans>This gift code has already been claimed.</Trans>}
			data-flx={`${prefix}.auth-error-state--2`}
		/>
	);
}
export const giftAuthPageConfig: ResourceAuthPageConfig<Gift> = {
	kind: 'gift',
	dataFlxPrefix: (mode) => `expressions.gift-${mode}-page`,
	useResource: (code, snapshot) => {
		const target = useMemo(() => instanceTargetFromSnapshot(snapshot), [snapshot.apiCodeVersion, snapshot.apiEndpoint]);
		const state = Gifts.getGift(code, target);
		useEffect(() => {
			if (!Gifts.getGift(code, target) && code) void GiftCommands.fetchWithCoalescing(code, target).catch(() => {});
		}, [code, target]);
		return {loading: !state || state.loading, error: Boolean(state?.error), resource: state?.data ?? null};
	},
	renderHeader: (gift, mode, sso) => (
		<GiftHeader gift={gift} variant={mode} data-flx={`expressions.gift-${mode}-page.gift-header${sso ? '.sso' : ''}`} />
	),
	renderBlockingState: (gift, prefix) =>
		gift.redeemed ? (
			<RedeemedGiftState
				prefix={prefix}
				data-flx="expressions.gift-auth-page-config.render-blocking-state.redeemed-gift-state"
			/>
		) : null,
	documentTitle: (i18n) => i18n._(CLAIM_GIFT_DESCRIPTOR),
	errorTitle: (i18n) => i18n._(GIFT_NOT_FOUND_TITLE_DESCRIPTOR),
	errorText: <Trans>This gift code may be invalid, expired, or already redeemed.</Trans>,
	registerPath: Routes.giftRegister,
	loginPath: (code, redirect) =>
		redirect ? setPathQueryParams(Routes.giftLogin(code), {redirect_to: redirect}) : Routes.giftLogin(code),
	redirectPath: (code, redirect) => setPathQueryParams(Routes.giftRegister(code), {redirect_to: redirect}),
	fallbackRedirectPath: () => '/',
	registerRedirectPath: () => '/',
	onComplete: (code, _i18n, snapshot) => GiftCommands.openAcceptModal(code, instanceTargetFromSnapshot(snapshot)),
	preferLogin: true,
	headerOutsideForm: true,
	submitLabel: <Trans>Create account to claim gift</Trans>,
};
