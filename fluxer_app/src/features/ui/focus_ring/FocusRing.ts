// SPDX-License-Identifier: AGPL-3.0-or-later

import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import FocusRingContext from '@app/features/ui/focus_ring/FocusRingContext';
import type {FocusRingProps} from '@app/features/ui/focus_ring/FocusRingTypes';
import {elementSupportsRef} from '@app/lib/react';
import type {ClassValue} from 'clsx';
import {clsx} from 'clsx';
import type {CSSProperties} from 'react';
import * as React from 'react';
import {useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

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

type FocusHandler = (event: React.FocusEvent<Element>) => unknown;

const useIsomorphicLayoutEffect = useLayoutEffect;
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
	const focusedRef = useRef(false);
	const mergedOnBlurRef = useRef<FocusHandler | undefined>(undefined);
	const mergedOnFocusRef = useRef<FocusHandler | undefined>(undefined);
	const [isFocusWithin, setFocusWithin] = useState(false);
	const ringContext = useContext(FocusRingContext);
	const child = React.Children.only(children) as React.ReactElement<FocusableChildProps & Record<string, unknown>>;
	const childProps = child.props as Record<string, unknown>;
	const supportsRef = elementSupportsRef(child);
	const childRef = supportsRef ? (childProps.ref as React.Ref<HTMLElement> | null) : null;
	const refs = supportsRef ? ([childRef, forwardedRef].filter(Boolean) as Array<React.Ref<HTMLElement>>) : [];
	const mergedRef = useMergeRefs(refs);
	const ringOptions = useMemo(
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
	useEffect(() => {
		if (!enabled) ringContext.hide();
	}, [enabled, ringContext]);
	useEffect(() => {
		return () => {
			if (focusedRef.current) ringContext.hide();
		};
	}, [ringContext]);
	useEffect(() => {
		const container = ringTarget?.current;
		if (focused == null || container == null) return;
		focusedRef.current = focused;
		if (focused) {
			ringContext.showForElement(container, ringOptions);
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
				ringContext.showForElement(container, ringOptions);
				return;
			}
			setFocusWithin(true);
			if (within) ringContext.showForElement(container, ringOptions);
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
	const updateBlurState = useCallback(() => {
		ringContext.hide();
		focusedRef.current = false;
		setFocusWithin(false);
	}, [ringContext]);
	const updateFocusState = useCallback(
		(event: React.FocusEvent<Element>) => {
			const container = ringTarget?.current;
			if (event.currentTarget === event.target) {
				focusedRef.current = true;
				ringContext.showForElement(container ?? event.currentTarget, ringOptions);
			} else {
				setFocusWithin(true);
				if (within) ringContext.showForElement(container ?? event.currentTarget, ringOptions);
			}
		},
		[ringTarget, within, ringContext, ringOptions],
	);
	const handleManagedBlur = useCallback(
		(event: React.FocusEvent<Element>) => {
			updateBlurState();
			mergedOnBlurRef.current?.(event);
		},
		[updateBlurState],
	);
	const handleManagedFocus = useCallback(
		(event: React.FocusEvent<Element>) => {
			updateFocusState(event);
			mergedOnFocusRef.current?.(event);
		},
		[updateFocusState],
	);
	const mergedChildProps: Record<string, unknown> = {...childProps};
	mergedChildProps['data-focus-ring-managed'] = 'true';
	if (supportsRef && refs.length > 0) {
		mergedChildProps.ref = mergedRef;
	}
	for (const [propKey, propValue] of Object.entries(passthroughProps as Record<string, unknown>)) {
		if (propKey === 'data-flx' && childProps['data-flx'] !== undefined) {
			continue;
		}
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
	const mergedOnBlur = mergedChildProps.onBlur as FocusHandler | undefined;
	const mergedOnFocus = mergedChildProps.onFocus as FocusHandler | undefined;
	mergedOnBlurRef.current = mergedOnBlur;
	mergedOnFocusRef.current = mergedOnFocus;
	mergedChildProps.onBlur = handleManagedBlur;
	mergedChildProps.onFocus = handleManagedFocus;
	return React.cloneElement(child, mergedChildProps);
});

FocusRing.displayName = 'FocusRing';

export default FocusRing;
