// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/messaging/components/modals/ExternalLinkWarningModal.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as TrustedDomainCommands from '@app/features/trusted_domain/commands/TrustedDomainCommands';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowRightIcon, WarningIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useRef, useState} from 'react';

const EXTERNAL_LINK_WARNING_DESCRIPTOR = msg({
	message: 'External link warning',
	comment: 'Warning text in the external link warning modal. Keep the tone plain and specific.',
});
const LEAVING_PRODUCT_DESCRIPTOR = msg({
	message: 'You are about to leave {productName}',
	comment: 'External link warning title. productName is the Fluxer product name.',
});
export const ExternalLinkWarningModal = observer(({url}: {url: string}) => {
	const {i18n} = useLingui();
	const [trustDomain, setTrustDomain] = useState(false);
	const initialFocusRef = useRef<HTMLButtonElement | null>(null);
	const hostname = useMemo(() => {
		try {
			return new URL(url).hostname;
		} catch {
			return url;
		}
	}, [url]);
	const handleContinue = useCallback(async () => {
		if (trustDomain) {
			await TrustedDomainCommands.addTrustedDomain(hostname);
		}
		void openExternalUrl(url);
		ModalCommands.pop();
	}, [url, hostname, trustDomain]);
	const handleCancel = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleTrustChange = useCallback((checked: boolean) => {
		setTrustDomain(checked);
	}, []);
	const title = i18n._(EXTERNAL_LINK_WARNING_DESCRIPTOR);
	return (
		<Modal.Root
			size="small"
			centered
			initialFocusRef={initialFocusRef}
			data-flx="messaging.external-link-warning-modal.modal-root"
		>
			<Modal.Header title={title} data-flx="messaging.external-link-warning-modal.modal-header" />
			<Modal.Content data-flx="messaging.external-link-warning-modal.modal-content">
				<div className={styles.content} data-flx="messaging.external-link-warning-modal.content">
					<div className={styles.iconContainer} data-flx="messaging.external-link-warning-modal.icon-container">
						<div className={styles.iconCircle} data-flx="messaging.external-link-warning-modal.icon-circle">
							<WarningIcon
								size={remFromPx(24)}
								className={styles.warningIcon}
								weight="fill"
								data-flx="messaging.external-link-warning-modal.warning-icon"
							/>
						</div>
						<div className={styles.textContainer} data-flx="messaging.external-link-warning-modal.text-container">
							<p className={styles.title} data-flx="messaging.external-link-warning-modal.title">
								{i18n._(LEAVING_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
							</p>
							<p className={styles.description} data-flx="messaging.external-link-warning-modal.description">
								<Trans>External links can be dangerous. Be careful.</Trans>
							</p>
						</div>
					</div>
					<div className={styles.urlSection} data-flx="messaging.external-link-warning-modal.url-section">
						<div className={styles.urlLabel} data-flx="messaging.external-link-warning-modal.url-label">
							<Trans>Destination URL:</Trans>
						</div>
						<div className={styles.urlBox} data-flx="messaging.external-link-warning-modal.url-box">
							<p className={styles.urlText} data-flx="messaging.external-link-warning-modal.url-text">
								{url}
							</p>
						</div>
					</div>
					<Checkbox
						checked={trustDomain}
						onChange={handleTrustChange}
						size="small"
						data-flx="messaging.external-link-warning-modal.checkbox.trust-change"
					>
						<span className={styles.checkboxLabel} data-flx="messaging.external-link-warning-modal.checkbox-label">
							<Trans>
								Always trust <strong data-flx="messaging.external-link-warning-modal.strong">{hostname}</strong> and
								skip this warning next time
							</Trans>
						</span>
					</Checkbox>
				</div>
			</Modal.Content>
			<Modal.Footer data-flx="messaging.external-link-warning-modal.modal-footer">
				<Button
					onClick={handleCancel}
					variant="secondary"
					className={styles.button}
					data-flx="messaging.external-link-warning-modal.button.cancel"
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					onClick={handleContinue}
					ref={initialFocusRef}
					variant="primary"
					className={styles.button}
					rightIcon={
						<ArrowRightIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="messaging.external-link-warning-modal.arrow-right-icon"
						/>
					}
					data-flx="messaging.external-link-warning-modal.button.continue"
				>
					<Trans>Visit site</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
