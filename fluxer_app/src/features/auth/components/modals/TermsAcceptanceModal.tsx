// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {acceptTerms} from '@app/features/terms/commands/TermsAcceptanceCommands';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {PRIVACY_POLICY_LAST_UPDATED, TERMS_OF_SERVICE_LAST_UPDATED} from '@fluxer/constants/src/PolicyConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useRef, useState} from 'react';

const UPDATED_POLICIES_DESCRIPTOR = msg({
	message: 'Updated policies',
	comment: 'Modal title shown when the user must accept changed legal policies.',
});
const I_AGREE_TO_THE_UPDATED_TERMS_DESCRIPTOR = msg({
	message: 'I agree to the updated terms',
	comment: 'Primary button label for accepting updated Terms of service.',
});
const I_AGREE_TO_THE_UPDATED_PRIVACY_POLICY_DESCRIPTOR = msg({
	message: 'I agree to the updated privacy policy',
	comment: 'Primary button label for accepting the updated Privacy policy.',
});
const I_AGREE_TO_THE_UPDATED_POLICIES_DESCRIPTOR = msg({
	message: 'I agree to the updated policies',
	comment: 'Primary button label for accepting both updated Terms of service and Privacy policy.',
});
const UPDATED_POLICIES_AGREEMENT_DESCRIPTOR = msg({
	message: 'By using {productName}, you agree to the updated policies.',
	comment: 'Legal notice in the updated policies modal. productName is the Fluxer product name.',
});

type UpdateKind = 'terms' | 'privacy' | 'both';

function getUpdateKind(termsAgreedAt: Date | null | undefined, privacyAgreedAt: Date | null | undefined): UpdateKind {
	const termsOutdated =
		TERMS_OF_SERVICE_LAST_UPDATED != null &&
		(!termsAgreedAt || termsAgreedAt.toISOString() < TERMS_OF_SERVICE_LAST_UPDATED);
	const privacyOutdated =
		PRIVACY_POLICY_LAST_UPDATED != null &&
		(!privacyAgreedAt || privacyAgreedAt.toISOString() < PRIVACY_POLICY_LAST_UPDATED);
	if (termsOutdated && privacyOutdated) return 'both';
	if (termsOutdated) return 'terms';
	return 'privacy';
}

export const TermsAcceptanceModal = observer(() => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const [submitting, setSubmitting] = useState(false);
	const primaryRef = useRef<HTMLButtonElement | null>(null);
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleAccept = useCallback(async () => {
		setSubmitting(true);
		try {
			await acceptTerms();
			ModalCommands.pop();
		} finally {
			setSubmitting(false);
		}
	}, []);
	if (!user) return null;
	const kind = getUpdateKind(user.termsAgreedAt, user.privacyAgreedAt);
	const termsUrl = Routes.terms();
	const privacyUrl = Routes.privacy();
	const productName = PRODUCT_NAME;
	return (
		<Modal.Root
			size="small"
			initialFocusRef={primaryRef}
			centered
			onClose={handleClose}
			data-flx="auth.terms-acceptance-modal.modal-root"
		>
			<Modal.Header
				title={i18n._(UPDATED_POLICIES_DESCRIPTOR)}
				onClose={handleClose}
				data-flx="auth.terms-acceptance-modal.modal-header"
			/>
			<Modal.Content data-flx="auth.terms-acceptance-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.terms-acceptance-modal.modal-content-layout">
					<Modal.Description className={markupStyles.markup} data-flx="auth.terms-acceptance-modal.modal-description">
						{kind === 'terms' && (
							<Trans>
								We've made significant changes to our{' '}
								<a
									href={termsUrl}
									target="_blank"
									rel="noopener noreferrer"
									className={markupStyles.link}
									data-flx="auth.terms-acceptance-modal.a"
								>
									Terms of service
								</a>
								. Review it before continuing to use {productName}.
							</Trans>
						)}
						{kind === 'privacy' && (
							<Trans>
								We've made significant changes to our{' '}
								<a
									href={privacyUrl}
									target="_blank"
									rel="noopener noreferrer"
									className={markupStyles.link}
									data-flx="auth.terms-acceptance-modal.a--2"
								>
									Privacy policy
								</a>
								. Review it before continuing to use {productName}.
							</Trans>
						)}
						{kind === 'both' && (
							<Trans>
								We've made significant changes to our{' '}
								<a
									href={termsUrl}
									target="_blank"
									rel="noopener noreferrer"
									className={markupStyles.link}
									data-flx="auth.terms-acceptance-modal.a--3"
								>
									Terms of service
								</a>{' '}
								and{' '}
								<a
									href={privacyUrl}
									target="_blank"
									rel="noopener noreferrer"
									className={markupStyles.link}
									data-flx="auth.terms-acceptance-modal.a--4"
								>
									Privacy policy
								</a>
								. Review them before continuing to use {productName}.
							</Trans>
						)}
					</Modal.Description>
					<Modal.Description
						className={markupStyles.markup}
						style={{marginTop: remFromPx(12), opacity: 0.7, fontSize: '0.85em'}}
						data-flx="auth.terms-acceptance-modal.modal-description--2"
					>
						{i18n._(UPDATED_POLICIES_AGREEMENT_DESCRIPTOR, {productName})}
					</Modal.Description>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="auth.terms-acceptance-modal.modal-footer">
				<Button onClick={handleClose} variant="secondary" data-flx="auth.terms-acceptance-modal.button.close">
					{i18n._(CLOSE_DESCRIPTOR)}
				</Button>
				<Button
					onClick={handleAccept}
					submitting={submitting}
					variant="primary"
					ref={primaryRef}
					data-flx="auth.terms-acceptance-modal.button.accept"
				>
					{kind === 'terms' && i18n._(I_AGREE_TO_THE_UPDATED_TERMS_DESCRIPTOR)}
					{kind === 'privacy' && i18n._(I_AGREE_TO_THE_UPDATED_PRIVACY_POLICY_DESCRIPTOR)}
					{kind === 'both' && i18n._(I_AGREE_TO_THE_UPDATED_POLICIES_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
