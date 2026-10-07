// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthLoginLayout} from '@app/features/auth/flow/AuthLoginLayout';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {AuthRuntimeTargetResetAction} from '@app/features/auth/flow/AuthRuntimeTargetResetAction';
import {isHandoffRequest} from '@app/features/auth/flow/auth_login_core/useDesktopHandoffFlow';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {DesktopHandoffMfaStep} from '@app/features/auth/flow/DesktopHandoffMfaStep';
import {GiftHeader} from '@app/features/auth/flow/GiftHeader';
import Authentication, {LoginState} from '@app/features/auth/state/Authentication';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import {fetchWithCoalescing, type Gift} from '@app/features/gift/commands/GiftCommands';
import Gifts from '@app/features/gift/state/Gifts';
import {
	GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR,
	GIFT_NOT_FOUND_TITLE_DESCRIPTOR,
} from '@app/features/gift/utils/GiftMessageDescriptors';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {GiftIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo} from 'react';

const GIFT_LOGIN_PAGE_STEP_ORDER: ReadonlyArray<LoginState> = [LoginState.DEFAULT, LoginState.MFA];

const CLAIM_GIFT_DESCRIPTOR = msg({
	message: 'Claim gift',
	comment: 'Action label on the gift redemption flow.',
});

interface GiftLoginPageProps {
	readonly code: string;
	readonly gift: Gift;
	readonly onLoginComplete: () => void;
}

const GiftLoginPage = observer(function GiftLoginPage({code, gift, onLoginComplete}: GiftLoginPageProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);
	const safeRedirect = safeRedirectTarget(params.get('redirect_to'));
	const isHandoff = isHandoffRequest(params);
	const registerSearch = safeRedirect == null ? undefined : {redirect_to: safeRedirect};
	const redirectPath = useMemo(() => {
		return setPathQueryParams(Routes.giftRegister(code), {redirect_to: safeRedirect});
	}, [code, safeRedirect]);
	return (
		<AuthLoginLayout
			redirectPath={redirectPath}
			inviteCode={null}
			desktopHandoff={isHandoff}
			excludeCurrentUser={false}
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
			forgotPasswordAction={null}
			showTitle={false}
			title={null}
			onBackActionChange={null}
			completeLoginRedirectPath={null}
			forceCredentials={false}
			startWithAddAccount={false}
			runtimeTarget={runtimeTarget}
			showInstanceSelector={null}
			ssoRedirectPath={null}
			suppressInlineBackButtons={false}
			initialIdentifier={null}
			registerLink={
				<AuthRouterLink
					to={Routes.giftRegister(code)}
					search={registerSearch}
					data-flx="expressions.gift-login-page.auth-router-link"
				>
					{i18n._(REGISTER_DESCRIPTOR)}
				</AuthRouterLink>
			}
			onLoginComplete={onLoginComplete}
			data-flx="expressions.gift-login-page.auth-login-layout"
		/>
	);
});
interface GiftLoginPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const GiftLoginPageContent = observer(function GiftLoginPageContent({runtimeSnapshot}: GiftLoginPageContentProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const loginState = Authentication.loginState;
	const {code} = useParams() as {code: string};
	const giftTarget = useMemo(
		() => instanceTargetFromSnapshot(runtimeSnapshot),
		[runtimeSnapshot.apiCodeVersion, runtimeSnapshot.apiEndpoint],
	);
	const handleLoginComplete = useCallback(async () => {
		await GiftCommands.openAcceptModal(code, giftTarget);
		runtimeTarget.reset();
	}, [code, giftTarget, runtimeTarget]);
	useFluxerDocumentTitle(i18n._(CLAIM_GIFT_DESCRIPTOR));
	const giftState = Gifts.getGift(code, giftTarget);
	useEffect(() => {
		const currentGiftState = Gifts.getGift(code, giftTarget);
		if (!currentGiftState && code) {
			void fetchWithCoalescing(code, giftTarget).catch(() => {});
		}
	}, [code, giftTarget]);
	if (!giftState || giftState.loading) {
		return <AuthLoadingState data-flx="expressions.gift-login-page.gift-login-page-container.auth-loading-state" />;
	}
	if (giftState.error || !giftState.data) {
		return (
			<AuthErrorState
				title={i18n._(GIFT_NOT_FOUND_TITLE_DESCRIPTOR)}
				text={<Trans>This gift code may be invalid, expired, or already redeemed.</Trans>}
				action={
					<AuthRuntimeTargetResetAction data-flx="expressions.gift-login-page.gift-login-page-container.auth-runtime-target-reset-action" />
				}
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
		case LoginState.DEFAULT:
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
						onLoginComplete={handleLoginComplete}
						data-flx="expressions.gift-login-page.gift-login-page-container.gift-login-page"
					/>
				</SteppedCarousel>
			);
		case LoginState.MFA:
			return (
				<SteppedCarousel
					step={loginState}
					steps={GIFT_LOGIN_PAGE_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(CLAIM_GIFT_DESCRIPTOR)}
					data-flx="expressions.gift-login-page.container-carousel"
				>
					<DesktopHandoffMfaStep
						fallbackRedirectPath="/"
						onLoginComplete={handleLoginComplete}
						data-flx="expressions.gift-login-page.gift-login-page-container.desktop-handoff-mfa-step"
					/>
				</SteppedCarousel>
			);
		default:
			return null;
	}
});

const GiftLoginPageContainer = observer(() => (
	<AuthRuntimeTargetGate data-flx="expressions.gift-login-page.runtime-target-gate">
		{(runtimeSnapshot) => (
			<GiftLoginPageContent
				runtimeSnapshot={runtimeSnapshot}
				data-flx="expressions.gift-login-page.gift-login-page-content"
			/>
		)}
	</AuthRuntimeTargetGate>
));

export default GiftLoginPageContainer;
