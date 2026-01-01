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

import {Trans, useLingui} from '@lingui/react/macro';
import {PaletteIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import * as ThemeActionCreators from '~/actions/ThemeActionCreators';
import {AuthErrorState} from '~/components/auth/AuthErrorState';
import {AuthLoadingState} from '~/components/auth/AuthLoadingState';
import {useDesktopHandoffFlow} from '~/components/auth/AuthLoginCore/useDesktopHandoffFlow';
import {AuthLoginLayout} from '~/components/auth/AuthLoginLayout';
import {AuthPageHeader} from '~/components/auth/AuthPageHeader';
import sharedStyles from '~/components/auth/AuthPageStyles.module.css';
import {AuthRouterLink} from '~/components/auth/AuthRouterLink';
import {DesktopDeepLinkPrompt} from '~/components/auth/DesktopDeepLinkPrompt';
import {HandoffCodeDisplay} from '~/components/auth/HandoffCodeDisplay';
import MfaScreen from '~/components/auth/MfaScreen';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import type {LoginSuccessPayload} from '~/hooks/useLoginFlow';
import {useThemeExists} from '~/hooks/useThemeExists';
import {useLocation, useParams} from '~/lib/router';
import {Routes} from '~/Routes';
import AccountManager from '~/stores/AccountManager';
import AuthenticationStore from '~/stores/AuthenticationStore';
import * as RouterUtils from '~/utils/RouterUtils';

const ThemeLoginPage = observer(function ThemeLoginPage() {
	const {t, i18n} = useLingui();
	const {themeId} = useParams() as {themeId: string};
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);

	const rawRedirect = params.get('redirect_to');
	const isDesktopHandoff = params.get('desktop_handoff') === '1';
	const redirectPath = useMemo(() => {
		const urlParams = new URLSearchParams();
		if (rawRedirect) {
			urlParams.set('redirect_to', rawRedirect);
		}
		return `${Routes.theme(themeId)}${urlParams.toString() ? `?${urlParams.toString()}` : ''}`;
	}, [themeId, rawRedirect]);

	const handleLoginComplete = useCallback(() => {
		if (!themeId) return;
		ThemeActionCreators.openAcceptModal(themeId, i18n);
	}, [themeId, i18n]);

	return (
		<AuthLoginLayout
			redirectPath={redirectPath}
			desktopHandoff={isDesktopHandoff}
			extraTopContent={
				<>
					<DesktopDeepLinkPrompt code={themeId} kind="theme" />
					<AuthPageHeader
						icon={
							<div className={sharedStyles.themeIconSpot}>
								<PaletteIcon className={sharedStyles.themeIcon} weight="fill" />
							</div>
						}
						title={t`You've got CSS!`}
						subtitle={t`Shared theme`}
					/>
				</>
			}
			showTitle={false}
			registerLink={
				<AuthRouterLink to={Routes.themeRegister(themeId)} search={{redirect_to: rawRedirect || undefined}}>
					<Trans>Register</Trans>
				</AuthRouterLink>
			}
			onLoginComplete={handleLoginComplete}
		/>
	);
});

const ThemeLoginPageMFA = observer(function ThemeLoginPageMFA() {
	const {i18n} = useLingui();
	const {themeId} = useParams() as {themeId: string};
	const location = useLocation();
	const params = useMemo(() => new URLSearchParams(location.search), [location.search]);

	const isDesktopHandoff = params.get('desktop_handoff') === '1';
	const rawRedirect = params.get('redirect_to');
	const redirectTo = isDesktopHandoff ? undefined : rawRedirect || Routes.theme(themeId);

	const mfaTicket = AuthenticationStore.currentMfaTicket;
	const mfaMethods = AuthenticationStore.availableMfaMethods;

	const hasStoredAccounts = AccountManager.orderedAccounts.length > 0;
	const handoff = useDesktopHandoffFlow({
		enabled: isDesktopHandoff,
		hasStoredAccounts,
		initialMode: 'idle',
	});

	const handleMfaSuccess = useCallback(
		async ({token, userId}: LoginSuccessPayload) => {
			if (isDesktopHandoff) {
				await handoff.start({token, userId});
				return;
			}

			await AuthenticationActionCreators.completeLogin({token, userId});
			ThemeActionCreators.openAcceptModal(themeId, i18n);
			AuthenticationActionCreators.clearMfaTicket();
			RouterUtils.replaceWith(redirectTo || '/');
		},
		[handoff, isDesktopHandoff, redirectTo, themeId, i18n],
	);

	const handleCancel = useCallback(() => {
		AuthenticationActionCreators.clearMfaTicket();
	}, []);

	if (!mfaTicket || !mfaMethods) {
		return null;
	}

	if (
		isDesktopHandoff &&
		(handoff.mode === 'generating' || handoff.mode === 'displaying' || handoff.mode === 'error')
	) {
		return (
			<HandoffCodeDisplay
				code={handoff.code}
				isGenerating={handoff.mode === 'generating'}
				error={handoff.mode === 'error' ? handoff.error : null}
				onRetry={handoff.retry}
			/>
		);
	}

	return (
		<MfaScreen challenge={{ticket: mfaTicket, ...mfaMethods}} onSuccess={handleMfaSuccess} onCancel={handleCancel} />
	);
});

const ThemeLoginPageContainer = observer(() => {
	const {t} = useLingui();
	const loginState = AuthenticationStore.loginState;
	const {themeId} = useParams() as {themeId: string};

	useFluxerDocumentTitle(t`Apply Theme`);

	const themeStatus = useThemeExists(themeId);

	if (themeStatus === 'loading') {
		return <AuthLoadingState />;
	}

	if (themeStatus === 'error') {
		return (
			<AuthErrorState
				title={<Trans>Theme not found</Trans>}
				text={<Trans>This theme may have been removed or the link is invalid.</Trans>}
			/>
		);
	}

	switch (loginState) {
		case 'default':
			return <ThemeLoginPage />;
		case 'mfa':
			return <ThemeLoginPageMFA />;
		default:
			return null;
	}
});

export default ThemeLoginPageContainer;
