// SPDX-License-Identifier: AGPL-3.0-or-later

import {tryInterceptChannelNavigationPath} from '@app/features/navigation/utils/ChannelNavigationGuard';
import type {
	Match,
	RouteComponentProps,
	RouteParams,
	Router,
	RouterProviderProps,
	RouterState,
	ScrollBehavior,
	SearchParamsInput,
} from '@app/features/platform/components/router/RouterTypes';
import * as React from 'react';
import {useCallback, useContext, useEffect, useMemo, useSyncExternalStore} from 'react';

const RouterContext = React.createContext<Router | null>(null);
export const RouterProvider: React.FC<RouterProviderProps> = ({router, children, linkContainerRef}) => {
	useEffect(() => {
		const container = linkContainerRef?.current ?? document.body;
		const onClick = (event: MouseEvent) => {
			if (event.defaultPrevented) return;
			if (event.button !== 0) return;
			if (event.metaKey || event.altKey || event.ctrlKey || event.shiftKey) return;
			const target = event.target as HTMLElement | null;
			if (!target || !('closest' in target)) return;
			const anchor = target.closest('a[href]') as HTMLAnchorElement | null;
			if (!anchor) return;
			if (anchor.hasAttribute('download')) return;
			if (anchor.getAttribute('rel') === 'external') return;
			if (anchor.target && anchor.target !== '_self') return;
			if (anchor.dataset.routerIgnore === 'true') return;
			const href = anchor.getAttribute('href');
			if (!href) return;
			if (href.startsWith('mailto:') || href.startsWith('tel:')) return;
			const url = new URL(anchor.href, window.location.href);
			if (url.origin !== window.location.origin) return;
			if (!router.canHandle(url.toString())) return;
			event.preventDefault();
			if (tryInterceptChannelNavigationPath(url.toString())) {
				return;
			}
			void router.navigate(url.toString(), {scroll: 'top'});
		};
		container.addEventListener('click', onClick);
		return () => container.removeEventListener('click', onClick);
	}, [router, linkContainerRef]);
	useEffect(
		() => () => {
			router.destroy();
		},
		[router],
	);
	return <RouterContext.Provider value={router}>{children ?? <Outlet />}</RouterContext.Provider>;
};

function useRouter(): Router {
	const ctx = useContext(RouterContext);
	if (!ctx) throw new Error('useRouter must be used within a RouterProvider');
	return ctx;
}

function useRouterState<T>(selector: (state: RouterState) => T): T {
	const router = useRouter();
	return useSyncExternalStore(
		useCallback((onChange) => router.subscribe(onChange), [router]),
		useCallback(() => selector(router.getState()), [router, selector]),
		useCallback(() => selector(router.getState()), [router, selector]),
	);
}

export function useLocation(): URL {
	return useRouterState((s) => s.location);
}

function useMatches(): Array<Match> {
	return useRouterState((s) => s.matches);
}

function useMatch(): Match | undefined {
	const matches = useMatches();
	return matches[matches.length - 1];
}

export function useParams(): RouteParams {
	const match = useMatch();
	return match?.params ?? {};
}

export interface LinkProps extends Omit<React.AnchorHTMLAttributes<HTMLAnchorElement>, 'href'> {
	to: string;
	search?: SearchParamsInput;
	hash?: string;
	state?: unknown;
	replace?: boolean;
	preload?: 'intent' | 'render' | 'none';
	scroll?: ScrollBehavior;
}

export const Link = React.forwardRef<HTMLAnchorElement, LinkProps>(function Link(
	{to, search, hash, state, replace, preload = 'intent', scroll, onClick, onPointerEnter, onFocus, ...rest},
	ref,
) {
	const router = useRouter();
	const location = useLocation();
	const hrefUrl = useMemo(() => router.resolveTo({to, search, hash}, location), [router, to, search, hash, location]);
	const handleIntentPreload = () => {
		if (preload === 'intent' && router.canHandle({to, search, hash}, location)) {
			void router.preload({to, search, hash});
		}
	};
	const handleClick = (event: React.MouseEvent<HTMLAnchorElement>) => {
		onClick?.(event);
		if (event.defaultPrevented) return;
		if (event.button !== 0) return;
		if (event.metaKey || event.altKey || event.ctrlKey || event.shiftKey) return;
		const target = event.currentTarget.getAttribute('target');
		if (target && target !== '_self') return;
		if (!router.canHandle(hrefUrl.toString(), location)) return;
		event.preventDefault();
		if (tryInterceptChannelNavigationPath(hrefUrl.toString())) {
			return;
		}
		void router.navigate({to, search, hash, state}, {replace, scroll});
	};
	const handlePointerEnter = (event: React.PointerEvent<HTMLAnchorElement>) => {
		onPointerEnter?.(event);
		if (!event.defaultPrevented) handleIntentPreload();
	};
	const handleFocus = (event: React.FocusEvent<HTMLAnchorElement>) => {
		onFocus?.(event);
		if (!event.defaultPrevented) handleIntentPreload();
	};
	return (
		<a
			data-flx="platform.router.react.link.a.click"
			{...rest}
			ref={ref}
			href={hrefUrl.toString()}
			onClick={handleClick}
			onPointerEnter={handlePointerEnter}
			onFocus={handleFocus}
			data-router-link="true"
		/>
	);
});
export const Outlet: React.FC = () => {
	const matches = useMatches();
	const location = useLocation();
	if (!matches.length) return null;
	const renderAt = (index: number): React.ReactNode => {
		const match = matches[index];
		const props: RouteComponentProps = {
			match,
			params: match.params,
			search: match.search,
			url: location,
		};
		const isLeaf = index === matches.length - 1;
		const child = isLeaf
			? match.route.component
				? React.createElement(match.route.component, props)
				: null
			: renderAt(index + 1);
		const Layout = match.route.layout;
		if (Layout) {
			return (
				<Layout data-flx="platform.router.react.render-at.layout" {...props}>
					{child}
				</Layout>
			);
		}
		return child;
	};
	return <>{renderAt(0)}</>;
};
