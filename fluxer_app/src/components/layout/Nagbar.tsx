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

import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {NativeDragRegion} from '~/components/layout/NativeDragRegion';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import styles from './Nagbar.module.css';

interface NagbarProps {
	isMobile: boolean;
	backgroundColor: string;
	textColor: string;
	children: React.ReactNode;
	onDismiss?: () => void;
	dismissible?: boolean;
}

export const Nagbar = observer(
	({isMobile, backgroundColor, textColor, children, onDismiss, dismissible = false}: NagbarProps) => {
		const showDismissButton = dismissible && onDismiss && !isMobile;

		return (
			<NativeDragRegion
				className={clsx(
					styles.nagbar,
					isMobile ? styles.nagbarMobile : styles.nagbarDesktop,
					showDismissButton && styles.nagbarDismissible,
				)}
				style={
					{
						backgroundColor,
						color: textColor,
						'--nagbar-background-color': backgroundColor,
					} as React.CSSProperties
				}
			>
				{children}
				{showDismissButton && (
					<FocusRing>
						<button
							type="button"
							className={styles.dismissButton}
							style={{color: textColor}}
							aria-label="Close"
							onClick={onDismiss}
						>
							<XIcon weight="regular" className={styles.dismissIcon} />
						</button>
					</FocusRing>
				)}
			</NativeDragRegion>
		);
	},
);
