// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/shared/StatusSlate.module.css';
import {Button} from '@app/features/ui/button/Button';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface StatusAction {
	text: React.ReactNode;
	onClick: () => void;
	variant?: React.ComponentProps<typeof Button>['variant'];
	fitContent?: boolean;
	submitting?: boolean;
}

interface StatusSlateProps {
	Icon: React.ComponentType<React.ComponentProps<'svg'>>;
	title: React.ReactNode;
	description: React.ReactNode;
	actions?: Array<StatusAction>;
	fullHeight?: boolean;
	iconClassName?: string;
	iconStyle?: React.CSSProperties;
}

export const StatusSlate: React.FC<StatusSlateProps> = observer(
	({Icon, title, description, actions = [], fullHeight = false, iconClassName, iconStyle}) => {
		const iconClass = [styles.icon, iconClassName].filter(Boolean).join(' ');
		return (
			<div
				className={`${styles.container} ${fullHeight ? styles.fullHeight : ''}`}
				data-flx="app.status-slate.container"
			>
				<Icon className={iconClass} style={iconStyle} aria-hidden data-flx="app.status-slate.icon" />
				<h3 className={styles.title} data-flx="app.status-slate.title">
					{title}
				</h3>
				<p className={styles.description} data-flx="app.status-slate.description">
					{description}
				</p>
				{actions.length > 0 && (
					<div className={styles.actions} data-flx="app.status-slate.actions">
						{actions.map((action, index) => (
							<Button
								key={index}
								variant={action.variant ?? 'primary'}
								fitContent={action.fitContent ?? true}
								onClick={action.onClick}
								disabled={action.submitting}
								submitting={action.submitting ?? false}
								data-flx="app.status-slate.button.click"
							>
								{action.text}
							</Button>
						))}
					</div>
				)}
			</div>
		);
	},
);
