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

import {observer} from 'mobx-react-lite';
import type React from 'react';

import styles from './AutocompleteItem.module.css';

export const AutocompleteItem = observer(
	({
		icon,
		name,
		description,
		isKeyboardSelected,
		isHovered,
		onSelect,
		onMouseEnter,
		onMouseLeave,
		innerRef,
		...props
	}: {
		icon?: React.ReactNode;
		name: React.ReactNode;
		description?: string;
		isKeyboardSelected: boolean;
		isHovered: boolean;
		onSelect: () => void;
		onMouseEnter: () => void;
		onMouseLeave: () => void;
		innerRef?: React.Ref<HTMLButtonElement>;
	} & React.HTMLAttributes<HTMLButtonElement>) => {
		const isActive = isKeyboardSelected || isHovered;
		return (
			<button
				type="button"
				className={styles.button}
				onClick={onSelect}
				onPointerEnter={onMouseEnter}
				onPointerLeave={onMouseLeave}
				ref={innerRef}
				{...props}
			>
				<div className={`${styles.container} ${isActive ? styles.selected : ''}`}>
					<div className={styles.content}>
						{icon && <div className={styles.icon}>{icon}</div>}
						<div className={styles.nameWrapper}>
							<div className={styles.name}>{name}</div>
						</div>
						{description && (
							<div className={styles.description}>
								<span>{description}</span>
							</div>
						)}
					</div>
				</div>
			</button>
		);
	},
);
