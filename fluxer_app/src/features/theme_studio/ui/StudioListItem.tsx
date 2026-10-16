// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioListItem.module.css';
import type React from 'react';
import type {ReactNode} from 'react';

interface StudioListItemProps {
	leading?: ReactNode;
	label: ReactNode;
	trailing?: ReactNode;
	codeBody?: ReactNode;
}

export const StudioListItem: React.FC<StudioListItemProps> = ({leading, label, trailing, codeBody}) => {
	const body = (
		<>
			{leading ? (
				<span className={styles.leading} data-flx="theme-studio.ui.studio-list-item.leading">
					{leading}
				</span>
			) : null}
			<span className={styles.text} data-flx="theme-studio.ui.studio-list-item.text">
				<span className={styles.label} data-flx="theme-studio.ui.studio-list-item.label">
					{label}
				</span>

				{codeBody ? (
					<code className={styles.code} data-flx="theme-studio.ui.studio-list-item.code">
						{codeBody}
					</code>
				) : null}
			</span>
			{trailing ? (
				<span className={styles.trailing} data-flx="theme-studio.ui.studio-list-item.trailing">
					{trailing}
				</span>
			) : null}
		</>
	);

	return (
		<div className={styles.item} data-flx="theme-studio.ui.studio-list-item.item">
			{body}
		</div>
	);
};
