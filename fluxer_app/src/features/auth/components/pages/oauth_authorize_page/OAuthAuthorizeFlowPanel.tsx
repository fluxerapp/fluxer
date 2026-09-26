// SPDX-License-Identifier: AGPL-3.0-or-later

import {OAuthAccountStep} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAccountStep';
import {OAuthCommunityStep} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthCommunityStep';
import {OAuthErrorState} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthErrorState';
import {OAuthLoadingState} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthLoadingState';
import {OAuthPermissionsStep} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthPermissionsStep';
import {OAuthScopesStep} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthScopesStep';
import {OAuthSuccessState} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthSuccessState';
import type {AuthorizeFlow} from '@app/features/auth/components/pages/oauth_authorize_page/state/useAuthorizeFlow';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const AUTHORIZE_APPLICATION_DESCRIPTOR = msg({
	message: 'Authorize application',
	comment: 'Short label in the authentication OAUTH authorize page. Keep the tone plain and specific.',
});

interface OAuthAuthorizeFlowPanelProps {
	flow: AuthorizeFlow;
	onDone?: () => void;
	showInlineActions?: boolean;
}

export const OAuthAuthorizeFlowPanel: React.FC<OAuthAuthorizeFlowPanelProps> = observer(
	({flow, onDone, showInlineActions = true}) => {
		const {i18n} = useLingui();
		if (flow.submitError) {
			return (
				<OAuthErrorState
					error={flow.submitError}
					validationError={null}
					data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.o-auth-error-state"
				/>
			);
		}
		switch (flow.phase.kind) {
			case 'loading':
				return (
					<OAuthLoadingState data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.o-auth-loading-state" />
				);
			case 'session_expired':
				return (
					<OAuthErrorState
						error={flow.sessionExpiredMessage}
						validationError={null}
						data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.o-auth-error-state--2"
					/>
				);
			case 'invalid_request':
				return (
					<OAuthErrorState
						error={flow.phase.message}
						validationError={null}
						data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.o-auth-error-state--3"
					/>
				);
			case 'success':
				return (
					<OAuthSuccessState
						destinationName={flow.phase.destinationName}
						onDone={onDone}
						data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.o-auth-success-state"
					/>
				);
			case 'review': {
				const phase = flow.phase;
				const authParams = flow.authParams;
				if (!authParams) {
					return (
						<OAuthErrorState
							error={null}
							validationError={null}
							data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.o-auth-error-state--4"
						/>
					);
				}
				const renderReviewStep = () => {
					if (phase.step === 'account') {
						return (
							<OAuthAccountStep
								clientLabel={flow.clientLabel}
								onCancel={flow.onCancel}
								onContinue={flow.goNext}
								data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.render-review-step.o-auth-account-step"
							/>
						);
					}
					if (phase.step === 'community') {
						return (
							<OAuthCommunityStep
								authParams={authParams}
								cannotSubmit={flow.cannotSubmit}
								destinationOptions={flow.destinationOptions}
								destinationsError={flow.destinationsError}
								destinationsLoading={flow.destinationsLoading}
								hasNextStep={flow.hasNextStep}
								hasPreviousStep={flow.hasPreviousStep}
								hasRequestedBotPermissions={flow.hasRequestedBotPermissions}
								onAuthorize={flow.onAuthorize}
								onBack={flow.goBack}
								onCancel={flow.onCancel}
								onNext={flow.goNext}
								onSelectDestination={flow.onSelectDestination}
								redirectHostname={flow.redirectHostname}
								selectedDestinationKey={flow.selectedDestinationKey}
								showInlineActions={showInlineActions}
								submitting={flow.submitting}
								data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.render-review-step.o-auth-community-step"
							/>
						);
					}
					if (phase.step === 'permissions') {
						return (
							<OAuthPermissionsStep
								authParams={authParams}
								botPermissionOptions={flow.permissionOptions}
								cannotSubmit={flow.cannotSubmit}
								clientLabel={flow.clientLabel}
								hasPreviousStep={flow.hasPreviousStep}
								onAuthorize={flow.onAuthorize}
								onGoBack={flow.goBack}
								onCancel={flow.onCancel}
								onNext={flow.goNext}
								onTogglePermission={flow.togglePermission}
								permissionsAdjusted={flow.permissionsAdjusted}
								redirectHostname={flow.redirectHostname}
								requestedPermissionKeys={flow.requestedPermissionKeys}
								requestsAdmin={flow.requestsAdmin}
								selectedPermissions={flow.selectedPermissions}
								showInlineActions={showInlineActions}
								submitting={flow.submitting}
								data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.render-review-step.o-auth-permissions-step"
							/>
						);
					}
					return (
						<OAuthScopesStep
							authParams={authParams}
							botInviteWithoutRedirect={flow.botInviteWithoutRedirect}
							cannotSubmit={flow.cannotSubmit}
							clientLabel={flow.clientLabel}
							hasNextStep={flow.hasNextStep}
							hasPreviousStep={flow.hasPreviousStep}
							hasBotScope={flow.hasBotScope}
							onAuthorize={flow.onAuthorize}
							onBack={flow.goBack}
							onCancel={flow.onCancel}
							onNext={flow.goNext}
							onToggleScope={flow.toggleScope}
							publicApp={flow.publicApp}
							redirectHostname={flow.redirectHostname}
							scopes={flow.scopes}
							scopesAdjusted={flow.scopesAdjusted}
							selectedScopes={flow.selectedScopes}
							showInlineActions={showInlineActions}
							isScopeLocked={flow.isScopeLocked}
							submitting={flow.submitting}
							data-flx="auth.oauth-authorize-page.o-auth-authorize-flow-panel.render-review-step.o-auth-scopes-step"
						/>
					);
				};
				return (
					<SteppedCarousel
						step={phase.step}
						steps={flow.reviewSteps}
						focusOnStepChange
						ariaLabel={i18n._(AUTHORIZE_APPLICATION_DESCRIPTOR)}
						data-flx="auth.o-auth-authorize-page.carousel"
					>
						{renderReviewStep()}
					</SteppedCarousel>
				);
			}
		}
	},
);
