// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/components/pages/OAuthAuthorizePage.module.css';
import type {AuthorizeParams} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizePageShared';
import type {AuthorizeFlow} from '@app/features/auth/components/pages/oauth_authorize_page/state/useAuthorizeFlow';
import {BACK_DESCRIPTOR, CANCEL_DESCRIPTOR, NEXT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const AUTHORIZE_DESCRIPTOR = msg({
	message: 'Authorize',
	comment: 'OAuth authorization primary button. Grants the selected OAuth access.',
});
const REDIRECT_AFTER_AUTHORIZING_DESCRIPTOR = msg({
	message: 'You will be taken to {redirectHostname} after authorizing.',
	comment: 'OAuth authorization footer shown before submission. {redirectHostname} is the external redirect host.',
});

type OAuthAuthorizeSubmittingState = 'approve' | 'deny' | null;

interface OAuthAuthorizeActionsProps {
	hasPreviousStep: boolean;
	hasNextStep: boolean;
	nextDisabled?: boolean;
	authorizeDisabled?: boolean;
	onAuthorize: () => void;
	onBack: () => void;
	onCancel: () => void;
	onNext: () => void;
	submitting: OAuthAuthorizeSubmittingState;
	layout?: 'footer' | 'inline';
	dataFlxPrefix: string;
}

const OAuthAuthorizeActionsBase: React.FC<OAuthAuthorizeActionsProps> = ({
	hasPreviousStep,
	hasNextStep,
	nextDisabled = false,
	authorizeDisabled = false,
	onAuthorize,
	onBack,
	onCancel,
	onNext,
	submitting,
	layout = 'footer',
	dataFlxPrefix,
}) => {
	const {i18n} = useLingui();
	const buttonClassName = layout === 'inline' ? styles.actionButton : undefined;
	const wrapButton = (kind: 'secondary' | 'primary', button: React.ReactNode) => {
		if (layout !== 'inline') {
			return button;
		}
		return (
			<div className={styles.actionButton} data-flx={`${dataFlxPrefix}.action-button.${kind}`}>
				{button}
			</div>
		);
	};
	const secondaryButton = (
		<Button
			type="button"
			variant="secondary"
			onClick={hasPreviousStep ? onBack : onCancel}
			disabled={submitting !== null}
			className={buttonClassName}
			data-flx={`${dataFlxPrefix}.button.${hasPreviousStep ? 'back' : 'cancel'}`}
		>
			{hasPreviousStep ? i18n._(BACK_DESCRIPTOR) : i18n._(CANCEL_DESCRIPTOR)}
		</Button>
	);
	const primaryButton = hasNextStep ? (
		<Button
			type="button"
			disabled={nextDisabled || submitting !== null}
			onClick={onNext}
			className={buttonClassName}
			data-flx={`${dataFlxPrefix}.button.next`}
		>
			{i18n._(NEXT_DESCRIPTOR)}
		</Button>
	) : (
		<Button
			type="button"
			disabled={authorizeDisabled || submitting === 'deny'}
			submitting={submitting === 'approve'}
			onClick={onAuthorize}
			className={buttonClassName}
			data-flx={`${dataFlxPrefix}.button.authorize`}
		>
			{i18n._(AUTHORIZE_DESCRIPTOR)}
		</Button>
	);
	return (
		<>
			{wrapButton('secondary', secondaryButton)}
			{wrapButton('primary', primaryButton)}
		</>
	);
};

export const OAuthAuthorizeActions = observer(OAuthAuthorizeActionsBase);

type OAuthAuthorizeInlineActionsProps = Omit<OAuthAuthorizeActionsProps, 'layout'>;

export const OAuthAuthorizeInlineActions: React.FC<OAuthAuthorizeInlineActionsProps> = observer((props) => (
	<div className={styles.actions} data-flx={`${props.dataFlxPrefix}.actions`}>
		<OAuthAuthorizeActions
			data-flx="auth.oauth-authorize-page.o-auth-authorize-actions.o-auth-authorize-inline-actions.o-auth-authorize-actions"
			{...props}
			layout="inline"
		/>
	</div>
));

interface OAuthAuthorizeRedirectNoticeProps {
	authParams: AuthorizeParams;
	redirectHostname: string | null;
	dataFlx: string;
	tooltipDataFlx: string;
	hostnameDataFlx: string;
}

export const OAuthAuthorizeRedirectNotice: React.FC<OAuthAuthorizeRedirectNoticeProps> = ({
	authParams,
	redirectHostname,
	dataFlx,
	tooltipDataFlx,
	hostnameDataFlx,
}) => {
	const {i18n} = useLingui();
	if (!redirectHostname) {
		return null;
	}
	return (
		<p className={styles.footerText} data-flx={dataFlx}>
			<Tooltip text={authParams.redirectUri ?? ''} maxWidth="xl" data-flx={tooltipDataFlx}>
				<span data-flx={hostnameDataFlx}>{i18n._(REDIRECT_AFTER_AUTHORIZING_DESCRIPTOR, {redirectHostname})}</span>
			</Tooltip>
		</p>
	);
};

interface OAuthAuthorizeActionSectionProps extends OAuthAuthorizeInlineActionsProps {
	authParams: AuthorizeParams;
	redirectHostname: string | null;
	showInlineActions: boolean;
	showRedirectNotice: boolean;
	dividerDataFlx: string;
	sectionDataFlx: string;
	redirectDataFlx: string;
	redirectTooltipDataFlx: string;
	redirectHostnameDataFlx: string;
}

export const OAuthAuthorizeActionSection: React.FC<OAuthAuthorizeActionSectionProps> = observer(
	({
		authParams,
		redirectHostname,
		showInlineActions,
		showRedirectNotice,
		dividerDataFlx,
		sectionDataFlx,
		redirectDataFlx,
		redirectTooltipDataFlx,
		redirectHostnameDataFlx,
		...actionsProps
	}) => {
		if (!showInlineActions && (!showRedirectNotice || !redirectHostname)) {
			return null;
		}
		return (
			<>
				<div className={styles.sectionDivider} data-flx={dividerDataFlx} />
				<div className={styles.actionSection} data-flx={sectionDataFlx}>
					{showInlineActions && (
						<OAuthAuthorizeInlineActions
							data-flx="auth.oauth-authorize-page.o-auth-authorize-actions.o-auth-authorize-action-section.o-auth-authorize-inline-actions"
							{...actionsProps}
						/>
					)}
					{showRedirectNotice && (
						<OAuthAuthorizeRedirectNotice
							authParams={authParams}
							redirectHostname={redirectHostname}
							dataFlx={redirectDataFlx}
							tooltipDataFlx={redirectTooltipDataFlx}
							hostnameDataFlx={redirectHostnameDataFlx}
							data-flx="auth.oauth-authorize-page.o-auth-authorize-actions.o-auth-authorize-action-section.o-auth-authorize-redirect-notice"
						/>
					)}
				</div>
			</>
		);
	},
);

export const OAuthAuthorizeFlowFooter: React.FC<{flow: AuthorizeFlow}> = observer(({flow}) => {
	if (flow.phase.kind !== 'review' || flow.phase.step === 'account') {
		return null;
	}
	return (
		<OAuthAuthorizeActions
			hasPreviousStep={flow.hasPreviousStep}
			hasNextStep={flow.hasNextStep}
			nextDisabled={flow.phase.step === 'community' && flow.cannotSubmit}
			authorizeDisabled={flow.cannotSubmit}
			onAuthorize={flow.onAuthorize}
			onBack={flow.goBack}
			onCancel={flow.onCancel}
			onNext={flow.goNext}
			submitting={flow.submitting}
			dataFlxPrefix="auth.o-auth-authorize-page.footer"
			data-flx="auth.oauth-authorize-page.o-auth-authorize-actions.o-auth-authorize-flow-footer.o-auth-authorize-actions"
		/>
	);
});
