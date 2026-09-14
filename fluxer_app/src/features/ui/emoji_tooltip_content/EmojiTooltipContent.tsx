// SPDX-License-Identifier: AGPL-3.0-or-later

import {Spinner} from '@app/features/ui/components/Spinner';
import styles from '@app/features/ui/emoji_tooltip_content/EmojiTooltipContent.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {clsx} from 'clsx';
import React from 'react';

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
						className={clsx(styles.emoji, emojiClassName)}
						data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.render-emoji.emoji"
					/>
				);
			}
			return null;
		};
		const content = (
			<>
				{renderEmoji()}
				{isLoading ? (
					<div
						className={clsx(styles.textContainer, styles.loading)}
						data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.text-container"
					>
						<Spinner data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.spinner" />
					</div>
				) : (
					<div
						className={styles.textContainer}
						data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.text-container--2"
					>
						{primaryContent}
						{subtext && (
							<div className={styles.subtext} data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.subtext">
								{subtext}
							</div>
						)}
					</div>
				)}
			</>
		);
		if (interactive && onClick) {
			return (
				<div
					ref={ref}
					className={clsx(styles.container, className)}
					data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.container"
				>
					<FocusRing offset={-2} data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.focus-ring">
						<button
							type="button"
							className={clsx(styles.inner, innerClassName)}
							onClick={onClick}
							data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.inner.click.button"
						>
							{content}
						</button>
					</FocusRing>
				</div>
			);
		}
		return (
			<div
				ref={ref}
				className={clsx(styles.container, className)}
				data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.container--2"
			>
				<div
					className={clsx(styles.inner, innerClassName)}
					data-flx="ui.emoji-tooltip-content.emoji-tooltip-content.inner"
				>
					{content}
				</div>
			</div>
		);
	},
);

EmojiTooltipContent.displayName = 'EmojiTooltipContent';
