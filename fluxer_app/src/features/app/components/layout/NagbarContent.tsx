// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/NagbarContent.module.css';
import {DISMISS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';

interface NagbarContentProps {
	message: React.ReactNode;
	actions?: React.ReactNode;
	isMobile: boolean;
	onDismiss?: () => void;
}

export const NagbarContent = ({message, actions, isMobile, onDismiss}: NagbarContentProps) => {
	const {i18n} = useLingui();
	const showMobileDismiss = isMobile && onDismiss;
	return (
		<div className={clsx(styles.container, isMobile && styles.containerMobile)} data-flx="app.nagbar-content.container">
			<p className={styles.message} dir="auto" data-flx="app.nagbar-content.message">
				{message}
			</p>
			{(actions || showMobileDismiss) && (
				<div className={clsx(styles.actions, isMobile && styles.actionsMobile)} data-flx="app.nagbar-content.actions">
					{showMobileDismiss && (
						<button
							type="button"
							className={styles.dismissButton}
							onClick={onDismiss}
							data-flx="app.nagbar-content.dismiss-button"
						>
							{i18n._(DISMISS_DESCRIPTOR)}
						</button>
					)}
					{actions}
				</div>
			)}
		</div>
	);
};
