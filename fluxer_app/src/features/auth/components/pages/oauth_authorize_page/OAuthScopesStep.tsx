// SPDX-License-Identifier: AGPL-3.0-or-later

import {getOAuth2ScopeDescription} from '@app/features/app/constants/AppConstants';
import styles from '@app/features/auth/components/pages/OAuthAuthorizePage.module.css';
import {OAuthAuthorizeActionSection} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizeActions';
import type {
	AuthorizeParams,
	PublicAppData,
} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizePageShared';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import type {OAuth2Scope} from '@fluxer/constants/src/OAuth2Constants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

const BOT_INVITE_NO_EXTERNAL_REDIRECT_DESCRIPTOR = msg({
	message: 'Bot invite (no external redirect)',
	comment: 'Short label in the authentication OAUTH scopes step. Keep the tone plain and specific.',
});
const BOT_SCOPE_REQUESTED_DESCRIPTOR = msg({
	message: 'Bot scope requested',
	comment: 'Short label in the authentication OAUTH scopes step. Keep the tone plain and specific.',
});
const REQUIRED_DESCRIPTOR = msg({
	message: 'Required',
	comment: 'Short label in the authentication OAUTH scopes step. Keep the tone plain and specific.',
});

interface OAuthScopesStepProps {
	authParams: AuthorizeParams;
	botInviteWithoutRedirect: boolean;
	cannotSubmit: boolean;
	clientLabel: string;
	hasNextStep: boolean;
	hasPreviousStep: boolean;
	hasBotScope: boolean;
	onAuthorize: () => void;
	onBack: () => void;
	onCancel: () => void;
	onNext: () => void;
	onToggleScope: (scope: string) => void;
	publicApp: PublicAppData | null;
	redirectHostname: string | null;
	scopes: ReadonlyArray<string>;
	scopesAdjusted: boolean;
	selectedScopes: ReadonlySet<string>;
	showInlineActions?: boolean;
	isScopeLocked: (scope: string) => boolean;
	submitting: 'approve' | 'deny' | null;
}

export const OAuthScopesStep: React.FC<OAuthScopesStepProps> = ({
	authParams,
	botInviteWithoutRedirect,
	cannotSubmit,
	clientLabel,
	hasNextStep,
	hasPreviousStep,
	hasBotScope,
	onAuthorize,
	onBack,
	onCancel,
	onNext,
	onToggleScope,
	publicApp,
	redirectHostname,
	scopes,
	scopesAdjusted,
	selectedScopes,
	showInlineActions = true,
	isScopeLocked,
	submitting,
}) => {
	const {i18n} = useLingui();
	const showRedirectNotice = Boolean(redirectHostname && !hasNextStep);
	return (
		<div className={styles.page} data-flx="auth.o-auth-authorize-page.page--3">
			<div className={styles.heroCard} data-flx="auth.o-auth-authorize-page.hero-card--2">
				<div className={styles.heroCopy} data-flx="auth.o-auth-authorize-page.hero-copy--2">
					<h1 className={styles.heroTitle} data-flx="auth.o-auth-authorize-page.hero-title--2">
						<Trans>{clientLabel} wants to connect</Trans>
					</h1>
					<p className={styles.heroDescription} data-flx="auth.o-auth-authorize-page.hero-description--2">
						{publicApp?.description ? (
							publicApp.description
						) : (
							<Trans>Review what this app is asking for before you continue.</Trans>
						)}
					</p>
					{botInviteWithoutRedirect || authParams.guildId || authParams.channelId || hasBotScope ? (
						<p className={styles.sectionDescription} data-flx="auth.o-auth-authorize-page.request-summary">
							{botInviteWithoutRedirect
								? i18n._(BOT_INVITE_NO_EXTERNAL_REDIRECT_DESCRIPTOR)
								: authParams.channelId
									? authParams.channelId
									: authParams.guildId
										? authParams.guildId
										: i18n._(BOT_SCOPE_REQUESTED_DESCRIPTOR)}
						</p>
					) : null}
				</div>
			</div>
			<div className={styles.sectionDivider} data-flx="auth.o-auth-authorize-page.section-divider--6" />
			<div className={styles.cardGrid} data-flx="auth.o-auth-authorize-page.card-grid">
				<div className={styles.panel} data-flx="auth.o-auth-authorize-page.panel">
					<div className={styles.sectionHeader} data-flx="auth.o-auth-authorize-page.section-header">
						<h3 className={styles.sectionTitle} data-flx="auth.o-auth-authorize-page.section-title">
							<Trans>Requested scopes</Trans>
						</h3>
						<p className={styles.sectionDescription} data-flx="auth.o-auth-authorize-page.section-description">
							<Trans>Turn off anything you're not comfortable with. Some features may stop working.</Trans>
						</p>
					</div>
					<div className={styles.scopeList} data-flx="auth.o-auth-authorize-page.scope-list">
						{scopes.length === 0 ? (
							<div className={styles.emptyState} data-flx="auth.o-auth-authorize-page.empty-state">
								<Trans>No specific scopes requested.</Trans>
							</div>
						) : (
							scopes.map((scope) => {
								const locked = isScopeLocked(scope);
								return (
									<div key={scope} className={styles.scopeRow} data-flx="auth.o-auth-authorize-page.scope-row">
										<Switch
											value={selectedScopes.has(scope)}
											onChange={() => onToggleScope(scope)}
											disabled={locked}
											compact
											label={
												<div className={styles.scopeHeading} data-flx="auth.o-auth-authorize-page.scope-heading">
													<span className={styles.scopeName} data-flx="auth.o-auth-authorize-page.scope-name">
														{scope}
													</span>
													{locked && (
														<span className={styles.scopeChip} data-flx="auth.o-auth-authorize-page.scope-chip">
															{i18n._(REQUIRED_DESCRIPTOR)}
														</span>
													)}
												</div>
											}
											description={
												<span
													className={styles.scopeDescription}
													data-flx="auth.o-auth-authorize-page.scope-description"
												>
													{getOAuth2ScopeDescription(i18n, scope as OAuth2Scope) ?? scope}
												</span>
											}
											data-flx="auth.o-auth-authorize-page.switch.toggle-scope"
										/>
									</div>
								);
							})
						)}
					</div>
					{scopes.length > 0 && selectedScopes.size === 0 ? (
						<div className={styles.caution} data-flx="auth.o-auth-authorize-page.caution--no-scopes">
							<Trans>Turn on at least one scope to authorize this app.</Trans>
						</div>
					) : (
						scopesAdjusted && (
							<div className={styles.caution} data-flx="auth.o-auth-authorize-page.caution--2">
								<Trans>Turning off scopes may prevent the app from working correctly.</Trans>
							</div>
						)
					)}
				</div>
			</div>
			<OAuthAuthorizeActionSection
				authParams={authParams}
				redirectHostname={redirectHostname}
				showInlineActions={showInlineActions}
				showRedirectNotice={showRedirectNotice}
				hasPreviousStep={hasPreviousStep}
				hasNextStep={hasNextStep}
				authorizeDisabled={cannotSubmit}
				onAuthorize={onAuthorize}
				onBack={onBack}
				onCancel={onCancel}
				onNext={onNext}
				submitting={submitting}
				dataFlxPrefix="auth.o-auth-authorize-page.scopes-step"
				dividerDataFlx="auth.o-auth-authorize-page.section-divider--9"
				sectionDataFlx="auth.o-auth-authorize-page.action-section--2"
				redirectDataFlx="auth.o-auth-authorize-page.footer-text--2"
				redirectTooltipDataFlx="auth.o-auth-authorize-page.tooltip--3"
				redirectHostnameDataFlx="auth.o-auth-authorize-page.redirect-hostname"
				data-flx="auth.oauth-authorize-page.o-auth-scopes-step.o-auth-authorize-action-section"
			/>
		</div>
	);
};
