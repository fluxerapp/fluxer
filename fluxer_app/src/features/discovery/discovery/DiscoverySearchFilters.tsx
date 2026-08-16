// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/discovery/discovery/DiscoverySearchFilters.module.css';
import Discovery from '@app/features/discovery/state/Discovery';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getCurrentLocale, getSortedDiscoveryLanguages} from '@app/features/user/utils/LocaleUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {animate} from 'animejs';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useCallback, useId, useLayoutEffect, useMemo, useRef} from 'react';

const SEARCH_FILTERS_DESCRIPTOR = msg({
	message: 'Search filters',
	comment: 'Accessible label for the filter sidebar shown beside the Discovery search results.',
});
const CATEGORIES_DESCRIPTOR = msg({
	message: 'Categories',
	comment: 'Heading above the category filter list in the Discovery search sidebar.',
});
const ALL_DESCRIPTOR = msg({
	message: 'All',
	comment: 'Label for the all-categories tab in the Discovery page.',
});
const ALL_LANGUAGES_DESCRIPTOR = msg({
	message: 'All languages',
	comment: 'Short label in the discovery page. Keep it concise.',
});
const FILTER_BY_LANGUAGE_DESCRIPTOR = msg({
	message: 'Filter by language',
	comment: 'Short label in the discovery page. Keep it concise.',
});
const CATEGORY_FILTER_DESCRIPTOR = msg({
	message: '{categoryLabel}, {count, plural, one {# community} other {# communities}}',
	comment:
		'Accessible label for a category row in the Discovery search sidebar. {categoryLabel} is the category name and {count} is how many communities match the current search in it. Both are inserted by code.',
});

const FILTERS_ENTER_TRANSLATE_Y_PX = 8;
const FILTERS_ENTER_DURATION_MS = 220;

interface DiscoveryCategoryFilterRow {
	readonly key: string;
	readonly categoryId: number | null;
	readonly label: string;
	readonly count: number;
}

interface DiscoverySearchFiltersProps {
	readonly onFilterApplied: () => void;
}

export const DiscoverySearchFilters = observer(function DiscoverySearchFilters({
	onFilterApplied,
}: DiscoverySearchFiltersProps) {
	const {i18n} = useLingui();
	const categoriesHeadingId = useId();
	const surfaceRef = useRef<HTMLElement | null>(null);
	const prefersReducedMotion = Accessibility.useReducedMotion;
	useLayoutEffect(() => {
		const surface = surfaceRef.current;
		if (surface == null || prefersReducedMotion) {
			return undefined;
		}
		const animation = animate(surface, {
			opacity: [0, 1],
			translateY: [FILTERS_ENTER_TRANSLATE_Y_PX, 0],
			duration: FILTERS_ENTER_DURATION_MS,
			ease: 'out(3)',
			onComplete: () => {
				surface.style.opacity = '';
				surface.style.transform = '';
			},
		});
		return () => {
			animation.revert();
			surface.style.opacity = '';
			surface.style.transform = '';
		};
	}, [prefersReducedMotion]);
	const languageOptions = useMemo<ReadonlyArray<ComboboxOption<string>>>(
		() => [
			{value: '', label: i18n._(ALL_LANGUAGES_DESCRIPTOR)},
			...getSortedDiscoveryLanguages().map((language) => ({
				value: language.code,
				label: language.label,
			})),
		],
		[i18n.locale],
	);
	const handleLanguageChange = useCallback(
		(value: string) => {
			void Discovery.search({language: value || null, offset: 0});
			onFilterApplied();
		},
		[onFilterApplied],
	);
	const handleCategoryClick = useCallback(
		(categoryId: number | null) => {
			void Discovery.search({category: categoryId, offset: 0});
			onFilterApplied();
		},
		[onFilterApplied],
	);
	const renderCategoryRow = (row: DiscoveryCategoryFilterRow): ReactNode => {
		const isActive = Discovery.category === row.categoryId;
		return (
			<FocusRing
				key={row.key}
				offset={-2}
				data-flx="discovery.discovery.discovery-search-filters.render-category-row.focus-ring"
			>
				<button
					type="button"
					className={clsx(styles.categoryItem, isActive && styles.categoryItemActive)}
					onClick={() => handleCategoryClick(row.categoryId)}
					aria-pressed={isActive}
					aria-label={i18n._(CATEGORY_FILTER_DESCRIPTOR, {categoryLabel: row.label, count: row.count})}
					data-flx="discovery.discovery.discovery-search-filters.render-category-row.category-item.category-click.button"
				>
					<span
						className={styles.categoryLabel}
						data-flx="discovery.discovery.discovery-search-filters.render-category-row.category-label"
					>
						{row.label}
					</span>
					<span
						className={styles.categoryCount}
						aria-hidden
						data-flx="discovery.discovery.discovery-search-filters.render-category-row.category-count"
					>
						{formatNumber(row.count, getCurrentLocale())}
					</span>
				</button>
			</FocusRing>
		);
	};
	const renderCategorySection = (): ReactNode => {
		const listedInAllCount = Discovery.listedInAllMatchCount;
		if (listedInAllCount == null) {
			return null;
		}
		const categoryRows: Array<DiscoveryCategoryFilterRow> = [
			{key: 'all', categoryId: null, label: i18n._(ALL_DESCRIPTOR), count: listedInAllCount},
			...Discovery.categories.map((category) => ({
				key: String(category.id),
				categoryId: category.id,
				label: category.name,
				count: Discovery.getCategoryMatchCount(category.id) ?? 0,
			})),
		];
		return (
			<div
				className={styles.section}
				data-flx="discovery.discovery.discovery-search-filters.render-category-section.section"
			>
				<h3
					id={categoriesHeadingId}
					className={styles.sectionHeading}
					data-flx="discovery.discovery.discovery-search-filters.render-category-section.section-heading"
				>
					{i18n._(CATEGORIES_DESCRIPTOR)}
				</h3>
				<div
					className={styles.categoryList}
					role="group"
					aria-labelledby={categoriesHeadingId}
					data-flx="discovery.discovery.discovery-search-filters.render-category-section.category-list"
				>
					{categoryRows.map(renderCategoryRow)}
				</div>
			</div>
		);
	};
	return (
		<aside
			ref={surfaceRef}
			className={styles.container}
			aria-label={i18n._(SEARCH_FILTERS_DESCRIPTOR)}
			data-flx="discovery.discovery.discovery-search-filters.container"
		>
			<div className={styles.section} data-flx="discovery.discovery.discovery-search-filters.section">
				<Combobox<string>
					label={i18n._(FILTER_BY_LANGUAGE_DESCRIPTOR)}
					options={languageOptions}
					value={Discovery.language ?? ''}
					onChange={handleLanguageChange}
					isSearchable
					data-flx="discovery.discovery.discovery-search-filters.combobox.language-change"
				/>
			</div>
			{renderCategorySection()}
		</aside>
	);
});
