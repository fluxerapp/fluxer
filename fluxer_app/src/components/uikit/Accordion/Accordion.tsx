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

import {CaretDownIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import sectionStyles from '~/components/modals/shared/SettingsSection.module.css';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import AccessibilityStore from '~/stores/AccessibilityStore';
import styles from './Accordion.module.css';

export interface AccordionProps {
	id: string;
	title: React.ReactNode;
	description?: React.ReactNode;
	defaultExpanded?: boolean;
	expanded?: boolean;
	onExpandedChange?: (expanded: boolean) => void;
	children: React.ReactNode;
	className?: string;
}

export const Accordion: React.FC<AccordionProps> = observer(
	({
		id,
		title,
		description,
		defaultExpanded = false,
		expanded: controlledExpanded,
		onExpandedChange,
		children,
		className,
	}) => {
		const [internalExpanded, setInternalExpanded] = React.useState(defaultExpanded);
		const headerRef = React.useRef<HTMLButtonElement>(null);
		const contentId = `${id}-content`;

		const isControlled = controlledExpanded !== undefined;
		const expanded = isControlled ? controlledExpanded : internalExpanded;
		const reduceMotion = AccessibilityStore.useReducedMotion;

		const handleToggle = React.useCallback(() => {
			const scrollContainer = headerRef.current?.closest('[data-settings-scroll-container]') as HTMLElement | null;
			const headerRect = headerRef.current?.getBoundingClientRect();
			const containerRect = scrollContainer?.getBoundingClientRect();
			const offsetFromContainer = headerRect && containerRect ? headerRect.top - containerRect.top : null;

			const wasAtBottom =
				scrollContainer != null &&
				Math.abs(scrollContainer.scrollHeight - scrollContainer.scrollTop - scrollContainer.clientHeight) < 20;

			const newExpanded = !expanded;

			if (isControlled) {
				onExpandedChange?.(newExpanded);
			} else {
				setInternalExpanded(newExpanded);
				onExpandedChange?.(newExpanded);
			}

			if (scrollContainer) {
				if (newExpanded && wasAtBottom) {
					setTimeout(() => {
						scrollContainer.scrollTop = scrollContainer.scrollHeight - scrollContainer.clientHeight;
					}, 250);
				} else if (offsetFromContainer !== null && !newExpanded) {
					requestAnimationFrame(() => {
						const newHeaderRect = headerRef.current?.getBoundingClientRect();
						const newContainerRect = scrollContainer.getBoundingClientRect();
						if (newHeaderRect && newContainerRect) {
							const newOffset = newHeaderRect.top - newContainerRect.top;
							const delta = newOffset - offsetFromContainer;
							if (Math.abs(delta) > 1) {
								scrollContainer.scrollTop += delta;
							}
						}
					});
				}
			}
		}, [expanded, isControlled, onExpandedChange]);

		const animationProps = reduceMotion
			? {}
			: {
					initial: {height: 0, opacity: 0},
					animate: {height: 'auto', opacity: 1},
					exit: {height: 0, opacity: 0},
					transition: {duration: 0.2, ease: [0.4, 0, 0.2, 1] as const},
				};

		return (
			<div className={clsx(styles.accordion, className)} id={id}>
				<FocusRing offset={-2}>
					<button
						ref={headerRef}
						type="button"
						className={styles.header}
						onClick={handleToggle}
						aria-expanded={expanded}
						aria-controls={contentId}
					>
						<div className={styles.headerContent}>
							<span className={sectionStyles.sectionTitle}>{title}</span>
							{description ? <span className={sectionStyles.sectionDescription}>{description}</span> : null}
						</div>
						<CaretDownIcon className={clsx(styles.caret, expanded && styles.caretExpanded)} size={20} weight="bold" />
					</button>
				</FocusRing>
				<AnimatePresence initial={false}>
					{expanded ? (
						<motion.div
							id={contentId}
							className={styles.contentWrapper}
							{...animationProps}
							style={{overflow: 'hidden'}}
						>
							<div className={styles.content}>{children}</div>
						</motion.div>
					) : null}
				</AnimatePresence>
			</div>
		);
	},
);
