// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveDisplayValue} from '@app/features/lexical/search/SearchFilterDisplay';
import {getSearchFilterMeta, SearchFilterCategory} from '@app/features/lexical/search/SearchFilterMeta';
import styles from '@app/features/lexical/search/SearchFilterPill.module.css';
import {SearchFilterPillSelections} from '@app/features/lexical/search/SearchFilterPillSelections';
import {Avatar} from '@app/features/ui/components/Avatar';
import Users from '@app/features/user/state/Users';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {useLexicalComposerContext} from '@lexical/react/LexicalComposerContext';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useSyncExternalStore} from 'react';

const SEARCH_USER_TAG_PATTERN = /^([A-Za-z0-9_]+)#(\d{4})$/;

const FILTER_DESCRIPTOR = msg({
	message: 'Search filter: {filter}',
	comment: 'Accessible label for a search filter pill. Preserve {filter}; it contains the filter operator and value.',
});
const EXCLUDED_FILTER_DESCRIPTOR = msg({
	message: 'Excluded search filter: {filter}',
	comment:
		'Accessible label for a negative search filter pill. Preserve {filter}; it contains the filter operator and value.',
});
const NOT_BADGE_DESCRIPTOR = msg({
	message: 'not',
	comment: 'Tiny badge shown on an excluded/negative search filter pill (e.g. -from:user). Keep it very short.',
});

interface SearchFilterPillProps {
	nodeKey: string;
	filterKey: string;
	value: string;
	exclude: boolean;
}

function resolvePillDisplayValue(
	category: SearchFilterCategory,
	value: string,
	untruncatedResolvedName: string | null,
): string {
	const resolvedDisplayValue = resolveDisplayValue(category, value, untruncatedResolvedName);
	if (category === SearchFilterCategory.USER) {
		return DisplayNameUtils.formatNameForStreamerMode(resolvedDisplayValue);
	}
	return resolvedDisplayValue;
}

function useIsPillSelected(nodeKey: string): boolean {
	const [editor] = useLexicalComposerContext();
	const subscribe = useCallback(
		(listener: () => void) => SearchFilterPillSelections.subscribe({editor, nodeKey, listener}),
		[editor, nodeKey],
	);
	const getSnapshot = useCallback(() => SearchFilterPillSelections.isSelected({editor, nodeKey}), [editor, nodeKey]);
	return useSyncExternalStore(subscribe, getSnapshot, () => false);
}

const SearchFilterPillComponent = ({nodeKey, filterKey, value, exclude}: SearchFilterPillProps) => {
	const {i18n} = useLingui();
	const isSelected = useIsPillSelected(nodeKey);
	const meta = getSearchFilterMeta(filterKey);
	const trimmedValue = value.trim();

	function resolveResolvedUser() {
		if (meta.category !== SearchFilterCategory.USER) {
			return null;
		}
		if (trimmedValue.toLowerCase() === '@me') {
			const currentUser = Users.getCurrentUser();
			if (currentUser == null) {
				return null;
			}
			return currentUser;
		}
		return resolveUser(trimmedValue);
	}
	const resolvedUser = resolveResolvedUser();
	let untruncatedResolvedUserName: string | null = null;
	if (resolvedUser != null) {
		untruncatedResolvedUserName = resolvedUser.displayName;
	}
	const displayValue = resolvePillDisplayValue(meta.category, trimmedValue, untruncatedResolvedUserName);
	const filterDescription = `${meta.operator}: ${displayValue}`;
	let pillLabel = i18n._(FILTER_DESCRIPTOR, {filter: filterDescription});
	if (exclude) {
		pillLabel = i18n._(EXCLUDED_FILTER_DESCRIPTOR, {filter: filterDescription});
	}
	const renderFilterIcon = (): React.ReactNode => {
		if (resolvedUser != null) {
			return (
				<Avatar user={resolvedUser} size={14} className={styles.avatar} disableStatusTooltip showOffline={false} />
			);
		}
		return <meta.Icon weight="bold" className={styles.icon} />;
	};

	return (
		<span
			role="img"
			className={clsx(styles.pill, exclude && styles.pillExclude, isSelected && styles.selected)}
			contentEditable={false}
			aria-label={pillLabel}
		>
			<span className={styles.visual} aria-hidden="true">
				{exclude && <span className={styles.notBadge}>{i18n._(NOT_BADGE_DESCRIPTOR)}</span>}
				{renderFilterIcon()}
				<span className={styles.operator}>{meta.operator}</span>
				<span className={styles.value}>{displayValue}</span>
			</span>
		</span>
	);
};

function resolveUser(value: string) {
	if (!SEARCH_USER_TAG_PATTERN.test(value)) {
		return null;
	}
	const user = Users.getUserByTag(value);
	if (user == null) {
		return null;
	}
	return user;
}

export const SearchFilterPill = observer(SearchFilterPillComponent);
