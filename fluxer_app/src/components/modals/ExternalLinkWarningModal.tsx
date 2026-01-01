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
import {ArrowRightIcon, WarningIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as TrustedDomainActionCreators from '~/actions/TrustedDomainActionCreators';
import styles from '~/components/modals/ExternalLinkWarningModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import {openExternalUrl} from '~/utils/NativeUtils';

export const ExternalLinkWarningModal = observer(({url, hostname}: {url: string; hostname: string}) => {
	const {t} = useLingui();
	const [trustDomain, setTrustDomain] = React.useState(false);
	const initialFocusRef = React.useRef<HTMLButtonElement | null>(null);

	const handleContinue = React.useCallback(() => {
		if (trustDomain) {
			TrustedDomainActionCreators.addTrustedDomain(hostname);
		}
		void openExternalUrl(url);
		ModalActionCreators.pop();
	}, [url, hostname, trustDomain]);

	const handleCancel = React.useCallback(() => {
		ModalActionCreators.pop();
	}, []);

	const handleTrustChange = React.useCallback((checked: boolean) => {
		setTrustDomain(checked);
	}, []);

	const title = t`External Link Warning`;

	return (
		<Modal.Root size="small" centered initialFocusRef={initialFocusRef}>
			<Modal.Header title={title} />
			<Modal.Content>
				<div className={styles.content}>
					<div className={styles.iconContainer}>
						<div className={styles.iconCircle}>
							<WarningIcon size={24} className={styles.warningIcon} weight="fill" />
						</div>
						<div className={styles.textContainer}>
							<p className={styles.title}>
								<Trans>You are about to leave Fluxer</Trans>
							</p>
							<p className={styles.description}>
								<Trans>External links can be dangerous. Please be careful.</Trans>
							</p>
						</div>
					</div>

					<div className={styles.urlSection}>
						<div className={styles.urlLabel}>
							<Trans>Destination URL:</Trans>
						</div>
						<div className={styles.urlBox}>
							<p className={styles.urlText}>{url}</p>
						</div>
					</div>

					<Checkbox checked={trustDomain} onChange={handleTrustChange} size="small">
						<span className={styles.checkboxLabel}>
							<Trans>
								Always trust <strong>{hostname}</strong> — skip this warning next time
							</Trans>
						</span>
					</Checkbox>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button onClick={handleCancel} variant="secondary" className={styles.button}>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					onClick={handleContinue}
					ref={initialFocusRef}
					variant="primary"
					className={styles.button}
					rightIcon={<ArrowRightIcon size={16} weight="bold" />}
				>
					<Trans>Visit Site</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
