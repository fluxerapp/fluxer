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
import {observer} from 'mobx-react-lite';
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import styles from './InlineEdit.module.css';

interface InlineEditProps {
	value: string;
	onSave: (value: string) => Promise<void> | void;
	prefix?: string;
	suffix?: string;
	placeholder?: string;
	maxLength?: number;
	validate?: (value: string) => boolean;
	className?: string;
	inputClassName?: string;
	buttonClassName?: string;
	width?: number | string;
	allowEmpty?: boolean;
}

type Mode = 'idle' | 'editing' | 'saving';

function sanitizeDraft(draft: string): string {
	return draft.replace(/[\r\n\t]/g, '');
}

export const InlineEdit: React.FC<InlineEditProps> = observer((props) => {
	const {
		className = '',
		inputClassName = '',
		buttonClassName = '',
		placeholder,
		width,
		onSave,
		value,
		prefix = '',
		suffix = '',
		maxLength,
		validate,
		allowEmpty = false,
	} = props;

	const [mode, setMode] = React.useState<Mode>('idle');
	const [draft, setDraft] = React.useState<string>(value);
	const [error, setError] = React.useState<string | null>(null);

	const editableRef = React.useRef<HTMLDivElement | null>(null);

	React.useEffect(() => {
		if (mode === 'idle') {
			setDraft(value);
		}
	}, [value, mode]);

	const fieldStyle = React.useMemo<React.CSSProperties | undefined>(() => {
		if (!width) return undefined;
		return {
			minWidth: typeof width === 'number' ? `${width}px` : width,
		};
	}, [width]);

	const canSave = (raw: string): boolean => {
		const trimmed = raw.trim();
		if (trimmed === value.trim()) return true;
		if (!trimmed.length) return !!allowEmpty;
		if (validate && !validate(trimmed)) return false;
		return true;
	};

	const startEdit = () => {
		setError(null);
		setDraft(value);
		setMode('editing');
	};

	React.useEffect(() => {
		if (mode !== 'editing') return;
		const el = editableRef.current;
		if (!el) return;

		el.textContent = draft;

		requestAnimationFrame(() => {
			el.focus();
			const range = document.createRange();
			range.selectNodeContents(el);
			range.collapse(false);
			const sel = window.getSelection();
			if (sel) {
				sel.removeAllRanges();
				sel.addRange(range);
			}
		});
	}, [mode]);

	const cancelEdit = () => {
		setError(null);
		setDraft(value);
		setMode('idle');
	};

	const doSave = async () => {
		const next = draft.trim();

		if (!canSave(next)) {
			setError('VALIDATION_FAILED');
			return;
		}

		if (next === value.trim()) {
			setMode('idle');
			return;
		}

		setMode('saving');
		setError(null);

		try {
			await Promise.resolve(onSave(next));
			setMode('idle');
		} catch (e: any) {
			setError(e?.message || 'SAVE_FAILED');
			setMode('editing');
		}
	};

	const handleEditableInput: React.FormEventHandler<HTMLDivElement> = (e) => {
		const el = e.currentTarget;
		const raw = el.textContent ?? '';
		let next = sanitizeDraft(raw);

		if (typeof maxLength === 'number' && maxLength > 0 && next.length > maxLength) {
			next = next.slice(0, maxLength);
		}

		if (next !== raw) {
			el.textContent = next;
			const range = document.createRange();
			range.selectNodeContents(el);
			range.collapse(false);
			const sel = window.getSelection();
			if (sel) {
				sel.removeAllRanges();
				sel.addRange(range);
			}
		}

		setError(null);
		setDraft(next);
	};

	const handleEditableKeyDown: React.KeyboardEventHandler<HTMLDivElement> = (e) => {
		if (e.key === 'Enter') {
			e.preventDefault();
			e.stopPropagation();
			void doSave();
			return;
		}

		if (e.key === 'Escape') {
			e.preventDefault();
			e.stopPropagation();
			cancelEdit();
			editableRef.current?.blur();
			return;
		}
	};

	const handleEditableBlur: React.FocusEventHandler<HTMLDivElement> = () => {
		if (!canSave(draft)) {
			cancelEdit();
		} else {
			void doSave();
		}
	};

	const isEditing = mode === 'editing' || mode === 'saving';
	const hasValue = value.trim().length > 0;
	const showPlaceholder = !hasValue && !isEditing;

	if (!isEditing) {
		return (
			<div className={clsx(styles.container, className)}>
				<FocusRing offset={-2}>
					<button
						type="button"
						onClick={startEdit}
						className={clsx(styles.idleButton, {[styles.placeholder]: showPlaceholder})}
						style={fieldStyle}
					>
						<span className={clsx(styles.wrapper, buttonClassName)}>
							{prefix && <span className={clsx(styles.inlineTextBase, styles.affix)}>{prefix}</span>}
							<span className={clsx(styles.inlineTextBase, styles.text, inputClassName)}>
								{hasValue ? value : placeholder || ''}
							</span>
							{suffix && <span className={clsx(styles.inlineTextBase, styles.affix)}>{suffix}</span>}
						</span>
					</button>
				</FocusRing>
				{error && <span className={styles.error}>{error}</span>}
			</div>
		);
	}

	return (
		<div className={clsx(styles.container, className)}>
			<div className={clsx(styles.wrapper, buttonClassName)} style={fieldStyle}>
				{prefix && <span className={clsx(styles.inlineTextBase, styles.affix)}>{prefix}</span>}
				{/* biome-ignore lint/a11y/useFocusableInteractive: contentEditable makes this focusable */}
				<div
					ref={editableRef}
					className={clsx(styles.inlineTextBase, styles.text, styles.editable, inputClassName)}
					contentEditable
					suppressContentEditableWarning
					onInput={handleEditableInput}
					onKeyDown={handleEditableKeyDown}
					onBlur={handleEditableBlur}
					data-placeholder={placeholder ?? ''}
					role="textbox"
				/>
				{suffix && <span className={clsx(styles.inlineTextBase, styles.affix)}>{suffix}</span>}
			</div>
			{error && <span className={styles.error}>{error}</span>}
		</div>
	);
});
