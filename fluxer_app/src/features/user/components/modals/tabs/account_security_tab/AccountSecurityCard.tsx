// SPDX-License-Identifier: AGPL-3.0-or-later

import statusStyles from '@app/features/app/components/dialogs/components/plutonium/PurchaseHistoryStatus.module.css';
import styles from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecurityCard.module.css';
import {clsx} from 'clsx';
import type React from 'react';
import {useId} from 'react';

type AccountSecurityStatusTone = 'success' | 'muted' | 'neutral' | 'pending';

interface AccountSecurityCardProps {
	title?: React.ReactNode;
	'aria-label'?: string;
	description?: React.ReactNode;
	status?: {label: React.ReactNode; tone: AccountSecurityStatusTone};
	action?: React.ReactNode;
	children?: React.ReactNode;
	'data-flx'?: string;
}

export const AccountSecurityCard: React.FC<AccountSecurityCardProps> = ({
	title,
	description,
	status,
	action,
	children,
	'aria-label': ariaLabel,
}) => {
	const titleId = useId();
	const hasHeader = title != null || action != null;
	return (
		<section
			className={clsx(styles.card, !hasHeader && styles.headerless)}
			aria-labelledby={title != null ? titleId : undefined}
			aria-label={title != null ? undefined : ariaLabel}
			data-flx="user.account-security-card.card"
		>
			{hasHeader ? (
				<div className={styles.header} data-flx="user.account-security-card.header">
					<div className={styles.headerText} data-flx="user.account-security-card.header-text">
						<div className={styles.titleRow} data-flx="user.account-security-card.title-row">
							{title != null ? (
								<h4 id={titleId} className={styles.title} data-flx="user.account-security-card.title">
									{title}
								</h4>
							) : null}
							{status ? (
								<span className={statusStyles[status.tone]} data-flx="user.account-security-card.status">
									{status.label}
								</span>
							) : null}
						</div>
						{description ? (
							<p className={styles.description} data-flx="user.account-security-card.description">
								{description}
							</p>
						) : null}
					</div>
					{action ? (
						<div className={styles.headerAction} data-flx="user.account-security-card.header-action">
							{action}
						</div>
					) : null}
				</div>
			) : null}
			{children ? (
				<div className={styles.rows} data-flx="user.account-security-card.rows">
					{children}
				</div>
			) : null}
		</section>
	);
};

interface AccountSecurityRowProps {
	label: React.ReactNode;
	description?: React.ReactNode;
	warning?: React.ReactNode;
	labelClassName?: string;
	children?: React.ReactNode;
	'data-flx'?: string;
}

export const AccountSecurityRow: React.FC<AccountSecurityRowProps> = ({
	label,
	description,
	warning,
	labelClassName,
	children,
}) => (
	<div className={styles.row} data-flx="user.account-security-card.row">
		<div className={styles.rowText} data-flx="user.account-security-card.row-text">
			<span className={labelClassName ?? styles.rowLabel} data-flx="user.account-security-card.row-label">
				{label}
			</span>
			{description ? (
				<span className={styles.rowDescription} data-flx="user.account-security-card.row-description">
					{description}
				</span>
			) : null}
			{warning ? (
				<span className={styles.rowWarning} data-flx="user.account-security-card.row-warning">
					{warning}
				</span>
			) : null}
		</div>
		{children ? (
			<div className={styles.rowActions} data-flx="user.account-security-card.row-actions">
				{children}
			</div>
		) : null}
	</div>
);

export const AccountSecurityEmptyRow: React.FC<{children: React.ReactNode; 'data-flx'?: string}> = ({children}) => (
	<div className={styles.emptyRow} data-flx="user.account-security-card.empty-row">
		{children}
	</div>
);

export const AccountSecuritySwitchRow: React.FC<{children: React.ReactNode; 'data-flx'?: string}> = ({children}) => (
	<div className={styles.switchRow} data-flx="user.account-security-card.switch-row">
		{children}
	</div>
);
