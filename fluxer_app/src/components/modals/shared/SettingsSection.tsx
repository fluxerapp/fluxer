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
import {Accordion} from '~/components/uikit/Accordion/Accordion';
import styles from './SettingsSection.module.css';

export interface SettingsSectionProps {
	id: string;
	title: React.ReactNode;
	description?: React.ReactNode;
	isAdvanced?: boolean;
	defaultExpanded?: boolean;
	children: React.ReactNode;
	className?: string;
}

export const SettingsSection: React.FC<SettingsSectionProps> = ({
	id,
	title,
	description,
	isAdvanced = false,
	defaultExpanded = true,
	children,
	className,
}) => {
	if (isAdvanced) {
		return (
			<Accordion
				id={id}
				title={title}
				description={description}
				defaultExpanded={defaultExpanded}
				className={className}
			>
				{children}
			</Accordion>
		);
	}

	return (
		<section id={id} className={clsx(styles.section, className)}>
			<div className={styles.sectionHeader}>
				<h3 className={styles.sectionTitle}>{title}</h3>
				{description ? <p className={styles.sectionDescription}>{description}</p> : null}
			</div>
			<div className={styles.sectionContent}>{children}</div>
		</section>
	);
};
