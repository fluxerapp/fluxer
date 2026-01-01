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

import type {ClassValue} from 'clsx';
import {clsx} from 'clsx';
import type {CSSProperties} from 'react';
import * as React from 'react';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import {elementSupportsRef} from '~/utils/react';
import FocusRingContext from './FocusRingContext';
import type {FocusRingProps} from './types';

type ForwardableProps = React.HTMLAttributes<Element>;

type FluxerFocusRingProps = FocusRingProps &
	ForwardableProps & {
		children: React.ReactElement;
	};

const EVENT_HANDLER_REGEX = /^on[A-Z]/;

interface FocusableChildProps extends React.HTMLAttributes<Element> {
	onFocus: (event: React.FocusEvent<Element>) => unknown;
	onBlur: (event: React.FocusEvent<Element>) => unknown;
}

const useIsomorphicLayoutEffect = React.useLayoutEffect;

const FocusRing = React.forwardRef<HTMLElement, FluxerFocusRingProps>(function FluxerFocusRing(
	{
		children,
		within = false,
		enabled = true,
		focused,
		offset = 0,
		focusTarget,
		ringTarget,
		ringClassName,
		focusClassName,
		focusWithinClassName,
		...passthroughProps
	},
	forwardedRef,
) {
	const focusedRef = React.useRef(false);
	const [isFocusWithin, setFocusWithin] = React.useState(false);
	const ringContext = React.useContext(FocusRingContext);

	const child = React.Children.only(children) as React.ReactElement<FocusableChildProps & Record<string, unknown>>;
	const childProps = child.props as Record<string, unknown>;
	const {onBlur: childOnBlur, onFocus: childOnFocus} = child.props;

	const supportsRef = elementSupportsRef(child);
	const childRef = supportsRef ? (childProps.ref as React.Ref<HTMLElement> | null) : null;
	const refs = supportsRef ? ([childRef, forwardedRef].filter(Boolean) as Array<React.Ref<HTMLElement>>) : [];
	const mergedRef = useMergeRefs(refs);

	const ringOptions = React.useMemo(
		() => ({
			className: ringClassName,
			offset,
		}),
		[ringClassName, offset],
	);

	useIsomorphicLayoutEffect(() => {
		if (!enabled) return;
		if (focusedRef.current || isFocusWithin) {
			ringContext.invalidate();
		}
	}, [enabled, ringContext, ringOptions, isFocusWithin]);

	React.useEffect(() => {
		if (!enabled) ringContext.hide();
	}, [enabled, ringContext]);

	React.useEffect(() => {
		return () => {
			if (focusedRef.current) ringContext.hide();
		};
	}, [ringContext]);

	React.useEffect(() => {
		const container = ringTarget?.current;
		if (focused == null || container == null) return;

		focusedRef.current = focused;
		if (focused) {
			ringContext.showElement(container, ringOptions);
		} else if (focused === false) {
			ringContext.hide();
		}
	}, [focused, ringOptions, ringContext, ringTarget]);

	useIsomorphicLayoutEffect(() => {
		if (focused != null) return;

		const target = focusTarget?.current;
		const container = ringTarget?.current;
		if (target == null || container == null) return;

		function onFocus(event: FocusEvent) {
			if (container == null) return;
			if (event.currentTarget === event.target) {
				focusedRef.current = true;
				ringContext.showElement(container, ringOptions);
				return;
			}

			setFocusWithin(true);
			if (within) ringContext.showElement(container, ringOptions);
		}

		function onBlur() {
			ringContext.hide();
			focusedRef.current = false;
			setFocusWithin(false);
		}

		(target as HTMLElement).addEventListener('focusin', onFocus, true);
		(target as HTMLElement).addEventListener('focusout', onBlur, true);

		return () => {
			(target as HTMLElement).removeEventListener('focusin', onFocus, true);
			(target as HTMLElement).removeEventListener('focusout', onBlur, true);
		};
	}, [within, ringOptions, focused, ringContext, focusTarget, ringTarget]);

	const onBlur = React.useCallback(
		(event: React.FocusEvent<Element>) => {
			ringContext.hide();
			focusedRef.current = false;
			setFocusWithin(false);
			childOnBlur?.(event);
		},
		[childOnBlur, ringContext],
	);

	const onFocus = React.useCallback(
		(event: React.FocusEvent<Element>) => {
			const container = ringTarget?.current;

			if (event.currentTarget === event.target) {
				focusedRef.current = true;
				ringContext.showElement(container ?? event.currentTarget, ringOptions);
			} else {
				setFocusWithin(true);
				if (within) ringContext.showElement(container ?? event.currentTarget, ringOptions);
			}

			childOnFocus?.(event);
		},
		[ringTarget, within, childOnFocus, ringContext, ringOptions],
	);

	const mergedChildProps: Record<string, unknown> = {...childProps};
	if (supportsRef && refs.length > 0) {
		mergedChildProps.ref = mergedRef;
	}

	for (const [propKey, propValue] of Object.entries(passthroughProps as Record<string, unknown>)) {
		if (propKey === 'className') {
			mergedChildProps.className = clsx(childProps.className as ClassValue, propValue as ClassValue);
			continue;
		}

		if (propKey === 'style') {
			mergedChildProps.style = {
				...(childProps.style as CSSProperties | undefined),
				...(propValue as CSSProperties | undefined),
			};
			continue;
		}

		if (EVENT_HANDLER_REGEX.test(propKey) && typeof propValue === 'function') {
			const existing = childProps[propKey];
			if (typeof existing === 'function') {
				mergedChildProps[propKey] = (...args: Array<unknown>) => {
					(propValue as (...params: Array<unknown>) => void)(...args);
					(existing as (...params: Array<unknown>) => void)(...args);
				};
			} else {
				mergedChildProps[propKey] = propValue;
			}
			continue;
		}

		mergedChildProps[propKey] = propValue;
	}

	if (!enabled || focusTarget != null || focused != null) {
		return React.cloneElement(child, mergedChildProps);
	}

	mergedChildProps.className = clsx(
		mergedChildProps.className as ClassValue,
		focusedRef.current ? focusClassName : undefined,
		isFocusWithin ? focusWithinClassName : undefined,
	);
	mergedChildProps.onBlur = onBlur;
	mergedChildProps.onFocus = onFocus;

	return React.cloneElement(child, mergedChildProps);
});

FocusRing.displayName = 'FocusRing';

export default FocusRing;
