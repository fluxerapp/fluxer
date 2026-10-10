// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRegisterFormCore} from '@app/features/auth/flow/AuthRegisterFormCore';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {AuthRuntimeTargetResetAction} from '@app/features/auth/flow/AuthRuntimeTargetResetAction';
import {AuthSsoPanel, resolveAuthPanelSso} from '@app/features/auth/flow/AuthSsoPanel';
import {DesktopDeepLinkPrompt} from '@app/features/auth/flow/DesktopDeepLinkPrompt';
import {GiftHeader} from '@app/features/auth/flow/GiftHeader';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import Gifts from '@app/features/gift/state/Gifts';
import {
	GIFT_ALREADY_REDEEMED_TITLE_DESCRIPTOR,
	GIFT_NOT_FOUND_TITLE_DESCRIPTOR,
} from '@app/features/gift/utils/GiftMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
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
interface GiftRegisterPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const GiftRegisterPageContent = observer(function GiftRegisterPageContent({
	runtimeSnapshot,
}: GiftRegisterPageContentProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
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
	const giftTarget = useMemo(
		() => instanceTargetFromSnapshot(runtimeSnapshot),
		[runtimeSnapshot.apiCodeVersion, runtimeSnapshot.apiEndpoint],
	);
	useFluxerDocumentTitle(i18n._(CLAIM_GIFT_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.STANDARD});
	const handleRegisterComplete = useCallback(
		async (response: AuthenticationCommands.TokenResponse) => {
			const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
			await AuthenticationCommands.completeLogin({
				token: response.token,
				userId: response.user_id,
				runtimeSnapshot,
				...(userData ? {userData} : {}),
			});
			await GiftCommands.openAcceptModal(code, giftTarget);
		},
		[code, giftTarget, runtimeSnapshot],
	);
	useEffect(() => {
		const currentGiftState = Gifts.getGift(code, giftTarget);
		if (!currentGiftState && code) {
			void GiftCommands.fetchWithCoalescing(code, giftTarget).catch(() => {});
		}
	}, [code, giftTarget]);
	const giftState = Gifts.getGift(code, giftTarget);
	if (!giftState || giftState.loading) {
		return <AuthLoadingState data-flx="expressions.gift-register-page.auth-loading-state" />;
	}
	if (giftState.error || !giftState.data) {
		return (
			<AuthErrorState
				title={i18n._(GIFT_NOT_FOUND_TITLE_DESCRIPTOR)}
				text={<Trans>This gift code may be invalid, expired, or already redeemed.</Trans>}
				action={
					<AuthRuntimeTargetResetAction data-flx="expressions.gift-register-page.auth-runtime-target-reset-action" />
				}
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
	const runtimeSso = resolveAuthPanelSso(runtimeSnapshot);
	if (runtimeSso?.enabled === true && runtimeSso.enforced === true) {
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
						runtimeSnapshot={runtimeSnapshot}
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
				<AuthRegisterFormCore
					fields={{showUsername: false}}
					submitLabel={<Trans>Create account to claim gift</Trans>}
					redirectPath="/"
					onRegister={handleRegisterComplete}
					runtimeSnapshot={runtimeSnapshot}
					onAuthenticated={runtimeTarget.reset}
					data-flx="expressions.gift-register-page.auth-minimal-register-form-core"
				/>
				<AuthBottomLink variant="login" to={loginPath} data-flx="expressions.gift-register-page.auth-bottom-link" />
			</div>
		</>
	);
});

const GiftRegisterPage = observer(() => (
	<AuthRuntimeTargetGate data-flx="expressions.gift-register-page.runtime-target-gate">
		{(runtimeSnapshot) => (
			<GiftRegisterPageContent
				runtimeSnapshot={runtimeSnapshot}
				data-flx="expressions.gift-register-page.gift-register-page-content"
			/>
		)}
	</AuthRuntimeTargetGate>
));

export default GiftRegisterPage;
