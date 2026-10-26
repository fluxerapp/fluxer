// SPDX-License-Identifier: AGPL-3.0-or-later

import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme_settings/components/QuickCssEditor.module.css';
import type {EditorStatusStore} from '@app/features/theme_settings/components/QuickCssStatusBar';
import {buildCssOutline, type OutlineEntry} from '@app/features/theme_settings/editor/CssOutline';
import {Input} from '@app/features/ui/components/form/FormInput';
import type {EditorView} from '@codemirror/view';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useEffect, useId, useMemo, useRef, useState, useSyncExternalStore} from 'react';

const FILTER_RULES_DESCRIPTOR = msg({
	message: 'Filter rules',
	comment: 'Placeholder of the filter field in the outline of the quick CSS editor, which lists the CSS rules.',
});
const NO_RULES_YET_DESCRIPTOR = msg({
	message: 'No rules yet.',
	comment: 'Shown in the outline of the quick CSS editor when the CSS has no rules.',
});
const NO_RULES_MATCH_DESCRIPTOR = msg({
	message: 'No rules match "{filter}".',
	comment: 'Shown in the outline of the quick CSS editor when no rule matches the filter.',
});
const OUTLINE_LIST_DESCRIPTOR = msg({
	message: 'Rules in this CSS',
	comment: 'Accessible name of the outline list in the quick CSS editor.',
});

const OUTLINE_REFRESH_DELAY_MS = 300;

interface QuickCssOutlineProps {
	view: EditorView;
	status: EditorStatusStore;
	beside: boolean;
	focusRequest: number;
	onClose: () => void;
}

export const QuickCssOutline: React.FC<QuickCssOutlineProps> = ({view, status, beside, focusRequest, onClose}) => {
	const {i18n} = useLingui();
	const inputRef = useRef<HTMLInputElement | null>(null);
	const revision = useSyncExternalStore(status.subscribe, () => status.get().revision);
	const [entries, setEntries] = useState<ReadonlyArray<OutlineEntry>>(() => buildCssOutline(view.state));
	const [filter, setFilter] = useState('');
	const [active, setActive] = useState(0);
	const listId = useId();
	const listRef = useRef<HTMLDivElement | null>(null);
	useEffect(() => {
		const timer = setTimeout(() => setEntries(buildCssOutline(view.state)), OUTLINE_REFRESH_DELAY_MS);
		return () => clearTimeout(timer);
	}, [view, revision]);
	useEffect(() => {
		if (focusRequest > 0) inputRef.current?.focus();
	}, [focusRequest]);
	const query = filter.trim().toLowerCase();
	const visible = useMemo(
		() => (query ? entries.filter((entry) => entry.label.toLowerCase().includes(query)) : entries),
		[entries, query],
	);
	const activeIndex = Math.min(active, Math.max(0, visible.length - 1));
	const goTo = (entry: OutlineEntry) => {
		view.dispatch({selection: {anchor: entry.from}, scrollIntoView: true});
		view.focus();
	};
	const moveActive = (next: number) => {
		const index = Math.max(0, Math.min(visible.length - 1, next));
		setActive(index);
		listRef.current?.querySelector(`#${CSS.escape(`${listId}-${index}`)}`)?.scrollIntoView({block: 'nearest'});
	};
	const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
		if (event.key === 'Escape') {
			event.preventDefault();
			onClose();
			return;
		}
		if (visible.length === 0) return;
		const moves: Record<string, number> = {
			ArrowDown: activeIndex + 1,
			ArrowUp: activeIndex - 1,
			PageDown: activeIndex + 10,
			PageUp: activeIndex - 10,
		};
		const next = moves[event.key];
		if (next !== undefined) {
			event.preventDefault();
			moveActive(next);
			return;
		}
		if (event.key === 'Enter') {
			event.preventDefault();
			goTo(visible[activeIndex]);
		}
	};
	let empty: string | null = null;
	if (entries.length === 0) empty = i18n._(NO_RULES_YET_DESCRIPTOR);
	else if (visible.length === 0) empty = i18n._(NO_RULES_MATCH_DESCRIPTOR, {filter: filter.trim()});
	return (
		<div
			className={clsx(styles.outline, beside ? styles.outlineBeside : styles.outlineAbove)}
			data-flx="theme-settings.quick-css-outline.root"
		>
			<div className={styles.outlineHeader} data-flx="theme-settings.quick-css-outline.header">
				<Input
					ref={inputRef}
					className={styles.outlineFilter}
					value={filter}
					onChange={(event) => {
						setFilter(event.target.value);
						setActive(0);
					}}
					onKeyDown={handleKeyDown}
					role="combobox"
					aria-expanded={visible.length > 0}
					aria-controls={listId}
					aria-autocomplete="list"
					aria-activedescendant={visible.length > 0 ? `${listId}-${activeIndex}` : undefined}
					placeholder={i18n._(FILTER_RULES_DESCRIPTOR)}
					aria-label={i18n._(FILTER_RULES_DESCRIPTOR)}
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(14)}
							weight="bold"
							data-flx="theme-settings.quick-css-outline.magnifying-glass-icon"
						/>
					}
					data-flx="theme-settings.quick-css-outline.filter"
				/>
				<button
					type="button"
					className={styles.toolButton}
					aria-label={i18n._(CLOSE_DESCRIPTOR)}
					onClick={onClose}
					data-flx="theme-settings.quick-css-outline.close"
				>
					<XIcon size={remFromPx(16)} weight="bold" data-flx="theme-settings.quick-css-outline.x-icon" />
				</button>
			</div>
			{empty ? (
				<p className={styles.outlineEmpty} data-flx="theme-settings.quick-css-outline.empty">
					{empty}
				</p>
			) : (
				<div
					ref={listRef}
					id={listId}
					role="listbox"
					className={styles.outlineList}
					aria-label={i18n._(OUTLINE_LIST_DESCRIPTOR)}
					data-flx="theme-settings.quick-css-outline.list"
				>
					{visible.map((entry, index) => (
						<button
							key={`${entry.from}:${entry.label}`}
							id={`${listId}-${index}`}
							type="button"
							role="option"
							tabIndex={-1}
							aria-selected={index === activeIndex}
							className={clsx(styles.outlineEntry, entry.kind === 'region' && styles.outlineRegion)}
							style={{paddingInlineStart: `${0.5 + Math.min(entry.depth, 6) * 0.75}rem`}}
							onMouseDown={(event) => event.preventDefault()}
							onClick={() => goTo(entry)}
							data-flx="theme-settings.quick-css-outline.entry"
						>
							{entry.label}
						</button>
					))}
				</div>
			)}
		</div>
	);
};
