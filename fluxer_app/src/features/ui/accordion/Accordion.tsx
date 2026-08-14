// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import sectionStyles from '@app/features/app/components/dialogs/shared/SettingsSection.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/accordion/Accordion.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useScrollAnchor} from '@app/features/ui/hooks/useScrollAnchor';
import {CaretDownIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useId, useState} from 'react';

export interface AccordionProps {
	id: string;
	title: React.ReactNode;
	description?: React.ReactNode;
	defaultExpanded?: boolean;
	expanded?: boolean;
	onExpandedChange?: (expanded: boolean) => void;
	children: React.ReactNode;
	className?: string;
	compact?: boolean;
	headerAction?: React.ReactNode;
}

const EXPAND_TRANSITION = {
	type: 'spring' as const,
	stiffness: 460,
	damping: 40,
	mass: 0.8,
};
const INSTANT_TRANSITION = {duration: 0};
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
		compact = false,
		headerAction,
	}) => {
		const [internalExpanded, setInternalExpanded] = useState(defaultExpanded);
		const {anchorRef, anchor} = useScrollAnchor<HTMLButtonElement>({
			containerSelector: '[data-settings-scroll-container]',
			durationMs: 500,
		});
		const contentId = `${id}-content`;
		const titleId = `${id}-title-${useId()}`;
		const isControlled = controlledExpanded !== undefined;
		const expanded = isControlled ? controlledExpanded : internalExpanded;
		const reducedMotion = Accessibility.useReducedMotion;
		const handleToggle = useCallback(() => {
			anchor();
			const newExpanded = !expanded;
			if (!isControlled) {
				setInternalExpanded(newExpanded);
			}
			onExpandedChange?.(newExpanded);
		}, [anchor, expanded, isControlled, onExpandedChange]);
		const transition = reducedMotion ? INSTANT_TRANSITION : EXPAND_TRANSITION;
		return (
			<div className={clsx(styles.accordion, className)} id={id} data-flx="ui.accordion.accordion.accordion">
				<div className={styles.headerRow} data-flx="ui.accordion.accordion.header-row">
					<div
						className={styles.headerClickTarget}
						onClick={handleToggle}
						aria-hidden={true}
						data-flx="ui.accordion.accordion.header-click-target.toggle.button"
					/>
					<div className={styles.headerContent} data-flx="ui.accordion.accordion.header-content">
						<div className={styles.titleRow} data-flx="ui.accordion.accordion.title-row">
							<FocusRing offset={-2} data-flx="ui.accordion.accordion.title.focus-ring">
								<button
									ref={anchorRef}
									type="button"
									className={styles.titleButton}
									onClick={handleToggle}
									aria-expanded={expanded}
									aria-controls={contentId}
									data-flx="ui.accordion.accordion.title-button.toggle.button"
								>
									<span
										id={titleId}
										className={compact ? styles.compactTitle : sectionStyles.sectionTitle}
										data-flx="ui.accordion.accordion.title"
									>
										{title}
									</span>
								</button>
							</FocusRing>
							{headerAction ? (
								<div className={styles.headerAction} data-flx="ui.accordion.accordion.header-action">
									{headerAction}
								</div>
							) : null}
						</div>
						{description ? (
							<button
								type="button"
								className={styles.descriptionButton}
								onClick={handleToggle}
								aria-expanded={expanded}
								aria-controls={contentId}
								aria-labelledby={titleId}
								data-flx="ui.accordion.accordion.description-button.toggle.button"
							>
								<span
									className={compact ? styles.compactDescription : sectionStyles.sectionDescription}
									data-flx="ui.accordion.accordion.description"
								>
									{description}
								</span>
							</button>
						) : null}
					</div>
					<FocusRing offset={-2} data-flx="ui.accordion.accordion.caret.focus-ring">
						<button
							type="button"
							className={styles.caretButton}
							onClick={handleToggle}
							aria-expanded={expanded}
							aria-controls={contentId}
							aria-labelledby={titleId}
							data-flx="ui.accordion.accordion.caret-button.toggle.button"
						>
							<CaretDownIcon
								className={clsx(styles.caret, expanded && styles.caretExpanded)}
								size={remFromPx(20)}
								weight="bold"
								data-flx="ui.accordion.accordion.caret"
							/>
						</button>
					</FocusRing>
				</div>
				<AnimatePresence initial={false} data-flx="ui.accordion.accordion.animate-presence">
					{expanded ? (
						<motion.div
							key="content"
							id={contentId}
							className={styles.contentWrapper}
							initial={{height: 0, opacity: 0}}
							animate={{height: 'auto', opacity: 1}}
							exit={{height: 0, opacity: 0}}
							transition={transition}
							data-flx="ui.accordion.accordion.content-wrapper"
						>
							<div className={styles.content} data-flx="ui.accordion.accordion.content">
								{children}
							</div>
						</motion.div>
					) : null}
				</AnimatePresence>
			</div>
		);
	},
);
