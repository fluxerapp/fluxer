// SPDX-License-Identifier: AGPL-3.0-or-later

import {calculateDistance, parseCoordinate} from '@app/api/utils/GeoUtils';
import {describe, expect, it} from 'vitest';

describe('calculateDistance', () => {
	it('returns 0 for identical coordinates', () => {
		expect(calculateDistance(0, 0, 0, 0)).toBe(0);
		expect(calculateDistance(40.7128, -74.006, 40.7128, -74.006)).toBe(0);
	});
	it('calculates distance between New York and Los Angeles', () => {
		const nyLat = 40.7128;
		const nyLon = -74.006;
		const laLat = 34.0522;
		const laLon = -118.2437;
		const distance = calculateDistance(nyLat, nyLon, laLat, laLon);
		expect(distance).toBeGreaterThan(3900);
		expect(distance).toBeLessThan(4000);
	});
	it('calculates distance between London and Paris', () => {
		const londonLat = 51.5074;
		const londonLon = -0.1278;
		const parisLat = 48.8566;
		const parisLon = 2.3522;
		const distance = calculateDistance(londonLat, londonLon, parisLat, parisLon);
		expect(distance).toBeGreaterThan(330);
		expect(distance).toBeLessThan(350);
	});
	it('calculates distance across the equator', () => {
		const distance = calculateDistance(10, 0, -10, 0);
		expect(distance).toBeGreaterThan(2200);
		expect(distance).toBeLessThan(2250);
	});
	it('calculates distance across the prime meridian', () => {
		const distance = calculateDistance(51.5, -0.1, 51.5, 0.1);
		expect(distance).toBeGreaterThan(10);
		expect(distance).toBeLessThan(20);
	});
	it('calculates distance across the international date line', () => {
		const distance = calculateDistance(0, 179, 0, -179);
		expect(distance).toBeGreaterThan(200);
		expect(distance).toBeLessThan(250);
	});
	it('handles negative latitudes (Southern Hemisphere)', () => {
		const sydneyLat = -33.8688;
		const sydneyLon = 151.2093;
		const melbourneLat = -37.8136;
		const melbourneLon = 144.9631;
		const distance = calculateDistance(sydneyLat, sydneyLon, melbourneLat, melbourneLon);
		expect(distance).toBeGreaterThan(700);
		expect(distance).toBeLessThan(750);
	});
	it('handles extreme latitudes near poles', () => {
		const distance = calculateDistance(89, 0, 89, 180);
		expect(distance).toBeGreaterThan(0);
		expect(distance).toBeLessThan(250);
	});
	it('calculates short distances accurately', () => {
		const distance = calculateDistance(40.7128, -74.006, 40.713, -74.007);
		expect(distance).toBeLessThan(1);
	});
	it('calculates very long distances (antipodal points)', () => {
		const distance = calculateDistance(0, 0, 0, 180);
		expect(distance).toBeGreaterThan(20000);
		expect(distance).toBeLessThan(20050);
	});
});

describe('parseCoordinate', () => {
	it('returns null for null input', () => {
		expect(parseCoordinate(null)).toBe(null);
	});
	it('returns null for undefined input', () => {
		expect(parseCoordinate(undefined)).toBe(null);
	});
	it('returns null for empty string', () => {
		expect(parseCoordinate('')).toBe(null);
	});
	it('parses positive integer coordinates', () => {
		expect(parseCoordinate('40')).toBe(40);
		expect(parseCoordinate('180')).toBe(180);
	});
	it('parses negative integer coordinates', () => {
		expect(parseCoordinate('-74')).toBe(-74);
		expect(parseCoordinate('-180')).toBe(-180);
	});
	it('parses positive decimal coordinates', () => {
		expect(parseCoordinate('40.7128')).toBe(40.7128);
		expect(parseCoordinate('0.5')).toBe(0.5);
	});
	it('parses negative decimal coordinates', () => {
		expect(parseCoordinate('-74.006')).toBe(-74.006);
		expect(parseCoordinate('-0.1278')).toBe(-0.1278);
	});
	it('parses zero', () => {
		expect(parseCoordinate('0')).toBe(0);
		expect(parseCoordinate('0.0')).toBe(0);
	});
	it('parses scientific notation', () => {
		expect(parseCoordinate('1e2')).toBe(100);
		expect(parseCoordinate('1.5e1')).toBe(15);
	});
	it('returns null for invalid strings', () => {
		expect(parseCoordinate('abc')).toBe(null);
		expect(parseCoordinate('NaN')).toBe(null);
	});
	it('returns null for Infinity', () => {
		expect(parseCoordinate('Infinity')).toBe(null);
		expect(parseCoordinate('-Infinity')).toBe(null);
	});
	it('parses coordinates with leading/trailing zeros', () => {
		expect(parseCoordinate('007.500')).toBe(7.5);
	});
});
