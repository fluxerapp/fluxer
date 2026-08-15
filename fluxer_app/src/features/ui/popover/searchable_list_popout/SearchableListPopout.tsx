// SPDX-License-Identifier: AGPL-3.0-or-later

import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import styles from '@app/features/ui/popover/searchable_list_popout/SearchableListPopout.module.css';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {
	type KeyboardEvent,
	type MouseEvent,
	type ReactNode,
	type RefCallback,
	useCallback,
	useEffect,
	useId,
	useMemo,
	useRef,
	useState,
} from 'react';

export interface SearchableListPopoutItem {
	id: string;
	ariaLabel: string;
	render: (props: {isActive: boolean; isSelected: boolean}) => ReactNode;
	searchValues: Array<string>;
	onSelect: () => void;
	onContextMenu?: (event: MouseEvent<HTMLButtonElement>) => void;
	isSelected?: boolean;
}

export interface SearchableListPopoutSection {
	id: string;
	heading?: ReactNode;
	items: Array<SearchableListPopoutItem>;
}

interface SearchableListPopoutProps {
	className?: string;
	defaultSearchQuery?: string;
	emptyStateClassName?: string;
	listAriaLabel: string;
	noResultsLabel: ReactNode;
	onRequestClose?: () => void;
	optionClassName?: string;
	placeholder: string;
	scrollerClassName?: string;
	searchClassName?: string;
	searchInputAriaLabel: string;
	sectionClassName?: string;
	sectionHeadingClassName?: string;
	sections: Array<SearchableListPopoutSection>;
	onSearchQueryChange?: (query: string) => void;
	disableInternalFiltering?: boolean;
}

interface FlattenedOption {
	id: string;
	option: SearchableListPopoutItem;
}

function getDefaultActiveIndex(options: Array<FlattenedOption>): number | null {
	if (options.length === 0) {
		return null;
	}
	const selectedIndex = options.findIndex((option) => option.option.isSelected);
	if (selectedIndex >= 0) {
		return selectedIndex;
	}
	return 0;
}

export function SearchableListPopout({
	className,
	defaultSearchQuery = '',
	emptyStateClassName,
	listAriaLabel,
	noResultsLabel,
	onRequestClose,
	optionClassName,
	placeholder,
	scrollerClassName,
	searchClassName,
	searchInputAriaLabel,
	sectionClassName,
	sectionHeadingClassName,
	sections,
	onSearchQueryChange,
	disableInternalFiltering = false,
}: SearchableListPopoutProps) {
	const inputRef = useRef<HTMLInputElement | null>(null);
	const optionRefs = useRef(new Map<string, HTMLButtonElement | null>());
	const [searchQuery, setSearchQuery] = useState(defaultSearchQuery);
	const [activeIndex, setActiveIndex] = useState<number | null>(null);
	const [hoveredOptionId, setHoveredOptionId] = useState<string | null>(null);
	const listId = useId();
	const filteredSections = useMemo(() => {
		if (disableInternalFiltering) {
			return sections.filter((section) => section.items.length > 0);
		}
		const normalizedSearchQuery = searchQuery.trim();
		if (!normalizedSearchQuery) {
			return sections;
		}
		const nextSections: Array<SearchableListPopoutSection> = [];
		for (const section of sections) {
			const filteredItems = matchSorter(section.items, normalizedSearchQuery, {
				keys: [(item) => item.searchValues],
			});
			if (filteredItems.length > 0) {
				nextSections.push({...section, items: filteredItems});
			}
		}
		return nextSections;
	}, [disableInternalFiltering, searchQuery, sections]);
	const flattenedOptions = useMemo(() => {
		const options: Array<FlattenedOption> = [];
		for (const section of filteredSections) {
			for (const option of section.items) {
				options.push({
					id: option.id,
					option,
				});
			}
		}
		return options;
	}, [filteredSections]);
	const activeOption = useMemo(() => {
		if (activeIndex === null) {
			return null;
		}
		return flattenedOptions[activeIndex] ?? null;
	}, [activeIndex, flattenedOptions]);
	const activeOptionDomId = activeOption ? `${listId}-option-${activeOption.id}` : undefined;
	useEffect(() => {
		requestAnimationFrame(() => {
			inputRef.current?.focus();
			inputRef.current?.select();
		});
	}, []);
	useEffect(() => {
		setActiveIndex((currentActiveIndex) => {
			if (flattenedOptions.length === 0) {
				return null;
			}
			if (currentActiveIndex === null || currentActiveIndex >= flattenedOptions.length) {
				return getDefaultActiveIndex(flattenedOptions);
			}
			return currentActiveIndex;
		});
	}, [flattenedOptions]);
	useEffect(() => {
		if (!activeOption) {
			return;
		}
		const optionNode = optionRefs.current.get(activeOption.id);
		optionNode?.scrollIntoView({block: 'nearest'});
	}, [activeOption]);
	const setOptionRef = useCallback((id: string): RefCallback<HTMLButtonElement> => {
		return (node) => {
			optionRefs.current.set(id, node);
		};
	}, []);
	const moveActiveIndex = useCallback(
		(delta: number) => {
			if (flattenedOptions.length === 0) {
				return;
			}
			setActiveIndex((currentActiveIndex) => {
				if (currentActiveIndex === null) {
					return getDefaultActiveIndex(flattenedOptions);
				}
				const nextIndex = currentActiveIndex + delta;
				if (nextIndex < 0) {
					return flattenedOptions.length - 1;
				}
				if (nextIndex >= flattenedOptions.length) {
					return 0;
				}
				return nextIndex;
			});
		},
		[flattenedOptions],
	);
	const handleInputKeyDown = useCallback(
		(event: KeyboardEvent<HTMLInputElement>) => {
			if (isIMEComposing(event)) {
				return;
			}
			switch (event.key) {
				case 'ArrowDown':
					event.preventDefault();
					moveActiveIndex(1);
					return;
				case 'ArrowUp':
					event.preventDefault();
					moveActiveIndex(-1);
					return;
				case 'Home':
					event.preventDefault();
					setActiveIndex(flattenedOptions.length > 0 ? 0 : null);
					return;
				case 'End':
					event.preventDefault();
					setActiveIndex(flattenedOptions.length > 0 ? flattenedOptions.length - 1 : null);
					return;
				case 'PageDown':
					event.preventDefault();
					moveActiveIndex(5);
					return;
				case 'PageUp':
					event.preventDefault();
					moveActiveIndex(-5);
					return;
				case 'Enter':
					if (!activeOption) {
						return;
					}
					event.preventDefault();
					activeOption.option.onSelect();
					return;
				case 'Escape':
					if (!onRequestClose) {
						return;
					}
					event.preventDefault();
					onRequestClose();
					return;
				default:
					return;
			}
		},
		[activeOption, flattenedOptions.length, moveActiveIndex, onRequestClose],
	);
	return (
		<div
			className={clsx(styles.popout, className)}
			data-flx="ui.popover.searchable-list-popout.searchable-list-popout.popout"
		>
			<div
				className={clsx(styles.search, searchClassName)}
				data-flx="ui.popover.searchable-list-popout.searchable-list-popout.search"
			>
				<Input
					autoFocus
					ref={inputRef}
					type="text"
					placeholder={placeholder}
					value={searchQuery}
					onChange={(event) => {
						setSearchQuery(event.target.value);
						setActiveIndex(null);
						setHoveredOptionId(null);
						onSearchQueryChange?.(event.target.value);
					}}
					onKeyDown={handleInputKeyDown}
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="ui.popover.searchable-list-popout.searchable-list-popout.magnifying-glass-icon"
						/>
					}
					role="combobox"
					aria-autocomplete="list"
					aria-expanded="true"
					aria-controls={listId}
					aria-label={searchInputAriaLabel}
					aria-activedescendant={activeOptionDomId}
					data-flx="ui.popover.searchable-list-popout.searchable-list-popout.combobox.set-search-query.text"
				/>
			</div>
			<Scroller
				className={clsx(styles.scroller, scrollerClassName)}
				key="searchable-list-popout-scroller"
				fade={false}
				data-flx="ui.popover.searchable-list-popout.searchable-list-popout.scroller"
			>
				<div
					role="listbox"
					id={listId}
					aria-label={listAriaLabel}
					className={styles.list}
					data-flx="ui.popover.searchable-list-popout.searchable-list-popout.list"
				>
					{filteredSections.length > 0 ? (
						filteredSections.map((section) => (
							<div
								className={clsx(styles.section, sectionClassName)}
								key={section.id}
								data-flx="ui.popover.searchable-list-popout.searchable-list-popout.section"
							>
								{section.heading && (
									<div
										className={clsx(styles.sectionHeading, sectionHeadingClassName)}
										data-flx="ui.popover.searchable-list-popout.searchable-list-popout.section-heading"
									>
										{section.heading}
									</div>
								)}
								{section.items.map((option) => {
									const domId = `${listId}-option-${option.id}`;
									const isKeyboardActive = activeOption?.id === option.id;
									const isHovered = hoveredOptionId === option.id;
									const isActive = isKeyboardActive || isHovered;
									const isSelected = option.isSelected ?? false;
									return (
										<button
											id={domId}
											key={option.id}
											ref={setOptionRef(option.id)}
											type="button"
											role="option"
											tabIndex={-1}
											aria-selected={isSelected}
											aria-label={option.ariaLabel}
											className={clsx(
												styles.option,
												isActive && styles.optionActive,
												isSelected && styles.optionSelected,
												optionClassName,
											)}
											onMouseEnter={() => setHoveredOptionId(option.id)}
											onMouseLeave={() =>
												setHoveredOptionId((currentId) => (currentId === option.id ? null : currentId))
											}
											onClick={option.onSelect}
											onContextMenu={option.onContextMenu}
											data-flx="ui.popover.searchable-list-popout.searchable-list-popout.option.select.button"
										>
											{option.render({isActive, isSelected})}
										</button>
									);
								})}
							</div>
						))
					) : (
						<div
							className={clsx(styles.emptyState, emptyStateClassName)}
							data-flx="ui.popover.searchable-list-popout.searchable-list-popout.empty-state"
						>
							{noResultsLabel}
						</div>
					)}
				</div>
			</Scroller>
		</div>
	);
}
