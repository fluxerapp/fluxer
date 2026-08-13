// SPDX-License-Identifier: AGPL-3.0-or-later

import {COPIED_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import styles from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetail.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckIcon, CopyIcon} from '@phosphor-icons/react';
import type React from 'react';

const APPLICATION_ID_DESCRIPTOR = msg({
	message: 'Application ID',
	comment: 'Short label in the application header. Keep it concise.',
});
const COPY_ID_DESCRIPTOR = msg({
	message: 'Copy ID',
	comment: 'Button or menu action label in the application header. Keep it concise.',
});

interface ApplicationHeaderProps {
	name: string;
	applicationId: string;
	onCopyId: () => void;
	idCopied: boolean;
}

export const ApplicationHeader: React.FC<ApplicationHeaderProps> = ({name, applicationId, onCopyId, idCopied}) => {
	const {i18n} = useLingui();
	return (
		<div
			className={styles.pageHeader}
			data-flx="user.applications-tab.application-detail.application-header.page-header"
		>
			<div className={styles.heroCard} data-flx="user.applications-tab.application-detail.application-header.hero-card">
				<div className={styles.heroTop} data-flx="user.applications-tab.application-detail.application-header.hero-top">
					<div data-flx="user.applications-tab.application-detail.application-header.div">
						<h2
							className={styles.heroTitle}
							data-flx="user.applications-tab.application-detail.application-header.hero-title"
						>
							{name}
						</h2>
						<Input
							label={i18n._(APPLICATION_ID_DESCRIPTOR)}
							value={applicationId}
							readOnly
							className={styles.metaInput}
							rightElement={
								<Button
									variant="secondary"
									compact
									fitContent
									onClick={onCopyId}
									leftIcon={
										idCopied ? (
											<CheckIcon
												size={remFromPx(14)}
												weight="bold"
												data-flx="user.applications-tab.application-detail.application-header.check-icon"
											/>
										) : (
											<CopyIcon
												size={remFromPx(14)}
												data-flx="user.applications-tab.application-detail.application-header.copy-icon"
											/>
										)
									}
									data-flx="user.applications-tab.application-detail.application-header.button.copy-id"
								>
									{idCopied ? i18n._(COPIED_DESCRIPTOR) : i18n._(COPY_ID_DESCRIPTOR)}
								</Button>
							}
							data-flx="user.applications-tab.application-detail.application-header.meta-input"
						/>
					</div>
				</div>
			</div>
		</div>
	);
};
