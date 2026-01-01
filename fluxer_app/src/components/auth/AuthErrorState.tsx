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

import type {Icon} from '@phosphor-icons/react';
import {QuestionIcon} from '@phosphor-icons/react';
import styles from './AuthPageStyles.module.css';

interface AuthErrorStateProps {
	icon?: Icon;
	title: React.ReactNode;
	text: React.ReactNode;
}

export function AuthErrorState({icon: IconComponent = QuestionIcon, title, text}: AuthErrorStateProps) {
	return (
		<div className={styles.errorContainer}>
			<div className={styles.errorIcon}>
				<IconComponent className={styles.errorIconSvg} />
			</div>
			<h1 className={styles.errorTitle}>{title}</h1>
			<p className={styles.errorText}>{text}</p>
		</div>
	);
}
