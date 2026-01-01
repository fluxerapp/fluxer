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
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Spinner} from '~/components/uikit/Spinner';
import styles from './EmojiTooltipContent.module.css';

interface EmojiTooltipContentProps {
	emoji?: React.ReactNode;
	emojiUrl?: string | null;
	emojiAlt?: string;
	emojiKey?: string;
	primaryContent?: React.ReactNode;
	subtext?: React.ReactNode;
	isLoading?: boolean;
	className?: string;
	emojiClassName?: string;
	innerClassName?: string;
	onClick?: () => void;
	interactive?: boolean;
}

export const EmojiTooltipContent = React.forwardRef<HTMLDivElement, EmojiTooltipContentProps>(
	(
		{
			emoji,
			emojiUrl,
			emojiAlt,
			emojiKey,
			primaryContent,
			subtext,
			isLoading = false,
			className,
			emojiClassName,
			innerClassName,
			onClick,
			interactive = false,
		},
		ref,
	) => {
		const renderEmoji = () => {
			if (emoji) {
				return emoji;
			}
			if (emojiUrl) {
				return (
					<img
						key={emojiKey}
						src={emojiUrl}
						alt={emojiAlt}
						draggable={false}
						className={clsx('emoji', styles.emoji, 'jumboable', emojiClassName)}
					/>
				);
			}
			return null;
		};

		const content = (
			<>
				{renderEmoji()}
				{isLoading ? (
					<div className={clsx(styles.textContainer, styles.loading)}>
						<Spinner />
					</div>
				) : (
					<div className={styles.textContainer}>
						{primaryContent}
						{subtext && <div className={styles.subtext}>{subtext}</div>}
					</div>
				)}
			</>
		);

		if (interactive && onClick) {
			return (
				<div ref={ref} className={clsx(styles.container, className)}>
					<FocusRing offset={-2}>
						<button type="button" className={clsx(styles.inner, innerClassName)} onClick={onClick}>
							{content}
						</button>
					</FocusRing>
				</div>
			);
		}

		return (
			<div ref={ref} className={clsx(styles.container, className)}>
				<div className={clsx(styles.inner, innerClassName)}>{content}</div>
			</div>
		);
	},
);

EmojiTooltipContent.displayName = 'EmojiTooltipContent';
