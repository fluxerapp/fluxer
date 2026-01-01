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
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Button} from '~/components/uikit/Button/Button';
import styles from './Slate.module.css';

interface SlateProps {
	icon: Icon;
	title: string;
	description: string;
	buttonText?: string;
	onClick?: () => void;
}

export const Slate: React.FC<SlateProps> = observer(({icon: Icon, title, description, buttonText, onClick}) => (
	<div className={styles.container}>
		<div className={styles.content}>
			<div className={styles.iconTextContainer}>
				<Icon className={styles.icon} />
				<div className={styles.textContainer}>
					<h3 className={styles.title}>{title}</h3>
					<p className={styles.description}>{description}</p>
				</div>
			</div>
			{buttonText && <Button onClick={onClick}>{buttonText}</Button>}
		</div>
	</div>
));
