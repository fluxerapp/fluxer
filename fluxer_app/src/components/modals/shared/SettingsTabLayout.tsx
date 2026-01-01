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

import {clsx} from 'clsx';
import type React from 'react';
import sectionStyles from './SettingsSection.module.css';
import styles from './SettingsTabLayout.module.css';

interface SettingsTabContainerProps {
	children: React.ReactNode;
	className?: string;
}

export const SettingsTabContainer: React.FC<SettingsTabContainerProps> = ({children, className}) => {
	return <div className={clsx(styles.container, className)}>{children}</div>;
};

interface SettingsTabHeaderProps {
	title: React.ReactNode;
	description?: React.ReactNode;
	className?: string;
}

export const SettingsTabHeader: React.FC<SettingsTabHeaderProps> = ({title, description, className}) => {
	return (
		<div className={clsx(styles.header, className)}>
			<h2 className={styles.title}>{title}</h2>
			{description && <p className={styles.description}>{description}</p>}
		</div>
	);
};

interface SettingsTabContentProps {
	children: React.ReactNode;
	className?: string;
}

export const SettingsTabContent: React.FC<SettingsTabContentProps> = ({children, className}) => {
	return <div className={clsx(styles.content, className)}>{children}</div>;
};

interface SettingsTabSectionProps {
	title?: React.ReactNode;
	description?: React.ReactNode;
	children: React.ReactNode;
	className?: string;
}

export const SettingsTabSection: React.FC<SettingsTabSectionProps> = ({title, description, children, className}) => {
	return (
		<div className={clsx(styles.subsection, className)}>
			{(title || description) && (
				<div className={sectionStyles.subsectionHeader}>
					{title && <h4 className={sectionStyles.subsectionTitle}>{title}</h4>}
					{description && <p className={sectionStyles.subsectionDescription}>{description}</p>}
				</div>
			)}
			<div className={sectionStyles.subsectionContent}>{children}</div>
		</div>
	);
};
