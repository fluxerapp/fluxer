// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {
	isApprovalFlowMode,
	isHandoffRequest,
	useDesktopHandoffFlow,
} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {GiftHeader} from '@app/features/auth/flow/GiftHeader';
import {ConnectedHandoffApprovalFlow} from '@app/features/auth/flow/HandoffApprovalFlow';
import MfaScreen from '@app/features/auth/flow/MfaScreen';
import AccountManager from '@app/features/auth/state/AccountManager';
import Authentication from '@app/features/auth/state/Authentication';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {safeRedirectTarget, safeRedirectTargetOrFallback} from '@app/features/auth/utils/SafeRedirect';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import {fetchWithCoalescing, type Gift} from '@app/features/gift/commands/GiftCommands';
import Gifts from '@app/features/gift/state/Gifts';
import {
	GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR,
	GIFT_NOT_FOUND_TITLE_DESCRIPTOR,
} from '@app/features/gift/utils/GiftMessageDescriptors';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
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
const GIFT_LOGIN_PAGE_STEP_ORDER = ['default', 'mfa'] as const;

interface GiftLoginPageProps {
	code: string;
	gift: Gift;
}

const GiftLoginPage = observer(function GiftLoginPage({code, gift}: GiftLoginPageProps) {
	const {i18n} = useLingui();
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const isHandoff = isHandoffRequest(params);
	const registerSearch = safeRedirect ? {redirect_to: safeRedirect} : undefined;
	const redirectPath = useMemo(() => {
		return setPathQueryParams(Routes.giftRegister(code), {redirect_to: safeRedirect});
	}, [code, safeRedirect]);
	const handleLoginComplete = useCallback(() => {
		GiftCommands.openAcceptModal(code);
	}, [code]);
	return (
		<AuthLoginLayout
			redirectPath={redirectPath}
			desktopHandoff={isHandoff}
			extraTopContent={
				<>
					<DesktopDeepLinkPrompt
						code={code}
						kind="gift"
						preferLogin={true}
						data-flx="expressions.gift-login-page.desktop-deep-link-prompt"
					/>
					<GiftHeader gift={gift} variant="login" data-flx="expressions.gift-login-page.gift-header" />
				</>
			}
			showTitle={false}
			registerLink={
				<AuthRouterLink
					to={Routes.giftRegister(code)}
					search={registerSearch}
					data-flx="expressions.gift-login-page.auth-router-link"
				>
					{i18n._(REGISTER_DESCRIPTOR)}
				</AuthRouterLink>
			}
			onLoginComplete={handleLoginComplete}
			data-flx="expressions.gift-login-page.auth-login-layout"
		/>
	);
});
const GiftLoginPageMFA = observer(function GiftLoginPageMFA() {
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const isHandoff = isHandoffRequest(params);
	const rawRedirect = params['get']('redirect_to');
	const redirectTo = isHandoff ? undefined : safeRedirectTargetOrFallback(rawRedirect, '/');
	const {code} = useParams() as {code: string};
	const mfaTicket = Authentication.currentMfaTicket;
	const mfaMethods = Authentication.availableMfaMethods;
	const hasStoredAccounts = AccountManager.orderedAccounts.length > 0;
	const handoff = useDesktopHandoffFlow({
		enabled: isHandoff,
		hasStoredAccounts,
		initialMode: 'idle',
	});
	const handleMfaSuccess = useCallback(
		async ({token, userId}: LoginSuccessPayload) => {
			if (isHandoff) {
				await AccountManager.refreshStoredAccount(userId, token);
				await handoff.start({token, userId});
				return;
			}
			await AuthenticationCommands.completeLogin({token, userId});
			GiftCommands.openAcceptModal(code);
			AuthenticationCommands.clearMfaTicket();
			RouterUtils.replaceWith(redirectTo || '/');
		},
		[handoff, isHandoff, redirectTo, code],
	);
	const handleCancel = useCallback(() => {
		AuthenticationCommands.clearMfaTicket();
	}, []);
	if (!mfaTicket || !mfaMethods) {
		return null;
	}
	if (isHandoff && isApprovalFlowMode(handoff.mode)) {
		return (
			<ConnectedHandoffApprovalFlow
				handoff={handoff}
				data-flx="expressions.gift-login-page.gift-login-page-mfa.connected-handoff-approval-flow"
			/>
		);
	}
	return (
		<MfaScreen
			challenge={{ticket: mfaTicket, ...mfaMethods}}
			onSuccess={handleMfaSuccess}
			onCancel={handleCancel}
			data-flx="expressions.gift-login-page.gift-login-page-mfa.mfa-screen"
		/>
	);
});
const GiftLoginPageContainer = observer(() => {
	const {i18n} = useLingui();
	const loginState = Authentication.loginState;
	const {code} = useParams() as {code: string};
	useFluxerDocumentTitle(i18n._(CLAIM_GIFT_DESCRIPTOR));
	const giftState = Gifts.gifts.get(code) ?? null;
	useEffect(() => {
		const currentGiftState = Gifts.gifts.get(code) ?? null;
		if (!currentGiftState && code) {
			void fetchWithCoalescing(code).catch(() => {});
		}
	}, [code]);
	if (!giftState || giftState.loading) {
		return <AuthLoadingState data-flx="expressions.gift-login-page.gift-login-page-container.auth-loading-state" />;
	}
	if (giftState.error || !giftState.data) {
		return (
			<AuthErrorState
				title={i18n._(GIFT_NOT_FOUND_TITLE_DESCRIPTOR)}
				text={<Trans>This gift code may be invalid, expired, or already redeemed.</Trans>}
				data-flx="expressions.gift-login-page.gift-login-page-container.auth-error-state"
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
				data-flx="expressions.gift-login-page.gift-login-page-container.auth-error-state--2"
			/>
		);
	}
	switch (loginState) {
		case 'default':
			return (
				<SteppedCarousel
					step={loginState}
					steps={GIFT_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(CLAIM_GIFT_DESCRIPTOR)}
					data-flx="expressions.gift-login-page.container-carousel"
				>
					<GiftLoginPage
						code={code}
						gift={gift}
						data-flx="expressions.gift-login-page.gift-login-page-container.gift-login-page"
					/>
				</SteppedCarousel>
			);
		case 'mfa':
			return (
				<SteppedCarousel
					step={loginState}
					steps={GIFT_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(CLAIM_GIFT_DESCRIPTOR)}
					data-flx="expressions.gift-login-page.container-carousel"
				>
					<GiftLoginPageMFA data-flx="expressions.gift-login-page.gift-login-page-container.gift-login-page-mfa" />
				</SteppedCarousel>
			);
		default:
			return null;
	}
});

export default GiftLoginPageContainer;
