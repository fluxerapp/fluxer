// SPDX-License-Identifier: AGPL-3.0-or-later

import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import styles from '@app/features/auth/flow/AuthPageStyles.module.css';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {Trans} from '@lingui/react/macro';
import type {ReactNode} from 'react';

type LegalConsentRequirement = 'terms' | 'privacy' | 'terms_and_privacy';

export interface RegistrationLegalConsentConfig {
	termsUrl: string | null;
	privacyUrl: string | null;
	requirement: LegalConsentRequirement | null;
}

function resolveLegalConsentRequirement(
	termsUrl: string | null,
	privacyUrl: string | null,
): LegalConsentRequirement | null {
	if (termsUrl != null && privacyUrl != null) {
		return 'terms_and_privacy';
	}
	if (termsUrl != null) {
		return 'terms';
	}
	if (privacyUrl != null) {
		return 'privacy';
	}
	return null;
}

export function getRegistrationLegalConsentConfig(
	runtimeSnapshot: RuntimeConfigSnapshot,
	showLegalConsent = true,
): RegistrationLegalConsentConfig {
	const isSelfHosted = DeveloperOptions.selfHostedModeOverride || runtimeSnapshot.features.self_hosted;
	const termsUrl =
		runtimeSnapshot.appPublic.legal.terms_url ?? (isSelfHosted ? null : `${runtimeSnapshot.marketingEndpoint}/terms`);
	const privacyUrl =
		runtimeSnapshot.appPublic.legal.privacy_url ??
		(isSelfHosted ? null : `${runtimeSnapshot.marketingEndpoint}/privacy`);
	if (!showLegalConsent) {
		return {termsUrl, privacyUrl, requirement: null};
	}
	return {termsUrl, privacyUrl, requirement: resolveLegalConsentRequirement(termsUrl, privacyUrl)};
}

function TermsLink({href, children}: {href: string; children?: ReactNode}) {
	return (
		<ExternalLink
			href={href}
			className={styles.policyLink}
			data-flx="auth.flow.registration-legal-notice.policy-link.terms"
		>
			{children}
		</ExternalLink>
	);
}

function PrivacyLink({href, children}: {href: string; children?: ReactNode}) {
	return (
		<ExternalLink
			href={href}
			className={styles.policyLink}
			data-flx="auth.flow.registration-legal-notice.policy-link.privacy"
		>
			{children}
		</ExternalLink>
	);
}

export function RegistrationLegalNotice({config}: {config: RegistrationLegalConsentConfig}) {
	const {termsUrl, privacyUrl, requirement} = config;
	let content: ReactNode = null;
	if (requirement === 'terms_and_privacy' && termsUrl !== null && privacyUrl !== null) {
		content = (
			<Trans comment="Notice right above the Create account button. Creating the account accepts the terms of service. The privacy policy is only explained, people never agree to it.">
				By creating an account, you agree to the{' '}
				<TermsLink href={termsUrl} data-flx="auth.flow.registration-legal-notice.terms-link">
					terms of service
				</TermsLink>
				. The{' '}
				<PrivacyLink href={privacyUrl} data-flx="auth.flow.registration-legal-notice.privacy-link">
					privacy policy
				</PrivacyLink>{' '}
				explains how we handle your data.
			</Trans>
		);
	} else if (requirement === 'terms' && termsUrl !== null) {
		content = (
			<Trans comment="Notice right above the Create account button when the instance only has terms of service.">
				By creating an account, you agree to the{' '}
				<TermsLink href={termsUrl} data-flx="auth.flow.registration-legal-notice.terms-link--2">
					terms of service
				</TermsLink>
				.
			</Trans>
		);
	} else if (requirement === 'privacy' && privacyUrl !== null) {
		content = (
			<Trans comment="Notice right above the Create account button when the instance only has a privacy policy. It only informs, people never agree to it.">
				The{' '}
				<PrivacyLink href={privacyUrl} data-flx="auth.flow.registration-legal-notice.privacy-link--2">
					privacy policy
				</PrivacyLink>{' '}
				explains how we handle your data.
			</Trans>
		);
	}
	if (content === null) return null;
	return (
		<p className={styles.legalNotice} dir="auto" data-flx="auth.flow.registration-legal-notice">
			{content}
		</p>
	);
}
