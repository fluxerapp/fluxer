// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthMinimalRegisterFormCore} from '@app/features/auth/flow/AuthMinimalRegisterFormCore';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthSsoPanel, isRuntimeSsoEnforced} from '@app/features/auth/flow/AuthSsoPanel';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {GiftHeader} from '@app/features/auth/flow/GiftHeader';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import Gifts from '@app/features/gift/state/Gifts';
import {
	GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR,
	GIFT_NOT_FOUND_TITLE_DESCRIPTOR,
} from '@app/features/gift/utils/GiftMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {GiftIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo} from 'react';

const CLAIM_GIFT_DESCRIPTOR = msg({
	message: 'Claim gift',
	comment: 'Action label on the gift redemption flow.',
});
const GiftRegisterPage = observer(function GiftRegisterPage() {
	const {i18n} = useLingui();
	const {code} = useParams() as {code: string};
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const ssoRedirectPath = useMemo(() => {
		return setPathQueryParams(Routes.giftRegister(code), {redirect_to: safeRedirect});
	}, [code, safeRedirect]);
	const loginPath = safeRedirect
		? setPathQueryParams(Routes.giftLogin(code), {redirect_to: safeRedirect})
		: Routes.giftLogin(code);
	useFluxerDocumentTitle(i18n._(CLAIM_GIFT_DESCRIPTOR));
	const giftState = Gifts.gifts.get(code) ?? null;
	const handleRegisterComplete = useCallback(
		async (response: AuthenticationCommands.TokenResponse) => {
			const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
			await AuthenticationCommands.completeLogin({
				token: response.token,
				userId: response.user_id,
				...(userData ? {userData} : {}),
			});
			GiftCommands.openAcceptModal(code);
		},
		[code],
	);
	useEffect(() => {
		const currentGiftState = Gifts.gifts.get(code) ?? null;
		if (!currentGiftState && code) {
			void GiftCommands.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code]);
	if (!giftState || giftState.loading) {
		return <AuthLoadingState data-flx="expressions.gift-register-page.auth-loading-state" />;
	}
	if (giftState.error || !giftState.data) {
		return (
			<AuthErrorState
				title={i18n._(GIFT_NOT_FOUND_TITLE_DESCRIPTOR)}
				text={<Trans>This gift code may be invalid, expired, or already redeemed.</Trans>}
				data-flx="expressions.gift-register-page.auth-error-state"
			/>
		);
	}
	const gift = giftState.data;
	if (gift.redeemed) {
		return (
			<AuthErrorState
				icon={GiftIcon}
				title={i18n._(GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR)}
				text={<Trans>This gift code has already been claimed.</Trans>}
				data-flx="expressions.gift-register-page.auth-error-state--2"
			/>
		);
	}
	if (isRuntimeSsoEnforced()) {
		return (
			<>
				<DesktopDeepLinkPrompt
					code={code}
					kind="gift"
					data-flx="expressions.gift-register-page.desktop-deep-link-prompt.sso"
				/>
				<GiftHeader gift={gift} variant="register" data-flx="expressions.gift-register-page.gift-header.sso" />
				<div className={sharedStyles.container} data-flx="expressions.gift-register-page.sso-container">
					<AuthSsoPanel
						redirectPath={ssoRedirectPath}
						dataFlx="expressions.gift-register-page.sso-panel"
						data-flx="expressions.gift-register-page.auth-sso-panel"
					/>
					<AuthBottomLink
						variant="login"
						to={loginPath}
						data-flx="expressions.gift-register-page.auth-bottom-link.sso"
					/>
				</div>
			</>
		);
	}
	return (
		<>
			<DesktopDeepLinkPrompt
				code={code}
				kind="gift"
				data-flx="expressions.gift-register-page.desktop-deep-link-prompt"
			/>
			<GiftHeader gift={gift} variant="register" data-flx="expressions.gift-register-page.gift-header" />
			<div className={sharedStyles.container} data-flx="expressions.gift-register-page.div">
				<AuthMinimalRegisterFormCore
					submitLabel={<Trans>Create account to claim gift</Trans>}
					redirectPath="/"
					onRegister={handleRegisterComplete}
					data-flx="expressions.gift-register-page.auth-minimal-register-form-core"
				/>
				<AuthBottomLink variant="login" to={loginPath} data-flx="expressions.gift-register-page.auth-bottom-link" />
			</div>
		</>
	);
});

export default GiftRegisterPage;
