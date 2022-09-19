/* Copyright (c) 2006-2007 Christopher J. W. Lloyd
   Copyright (c) 2022 Zoe Knox <zoe@pixin.net>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */
#import <AppKit/AppKit.h>
#import <CoreGraphics/CGWindow.h>
#import <CoreGraphics/CGGeometry.h>
#import <Foundation/Foundation.h>
#ifdef WIN32
#import <windows.h>
#import <AppKit/Win32Window.h>
#endif
#import <AppKit/NSStatusItem+Private.h>

@implementation NSStatusBar

static NSStatusBar *_statusBar=nil;

+ (NSStatusBar *)systemStatusBar{
    if(_statusBar==nil){
        _statusBar = [[NSStatusBar alloc] init];
    }
    return _statusBar;
}

- (id)init {
    self = [super init];
    if (self) {
        _statusItems = [[NSMutableArray alloc] init];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(handleApplicationClose) name:@"NSApplicationWillTerminateNotification" object:nil];
    }
    return self;
}

- (BOOL)isVertical{
    return NO; // menuBar cannot be vertical
}

- (void)removeStatusItem:(NSStatusItem *)item{
    
}

- (NSStatusItem *)statusItemWithLength:(CGFloat)length{
    NSStatusItem *item = [[NSStatusItem alloc] init];
    [item setLength:length];
    [_statusItems addObject:item];
    return [item autorelease];
}
- (CGFloat)thickness{
    return 22; // keep this in sync with menuBarHeight in SystemUIServer/desktop.h
}

#ifdef WIN32
- (void)_trayNotificationForID:(int)aTrayID event:(int)anEvent{
    for(int i = 0; i < _statusItems.count; i++){
        NSStatusItem *item = [_statusItems objectAtIndex:i];
        if([item trayIconID] == aTrayID){
            [item _processWin32Event:anEvent];
        }
    }
}

- (Win32Window *)fakeWindow{
    if(!_fakeWindow){
        _fakeWindow = [[Win32Window alloc] initWithFrame:CGRectMake(0, 0, 1, 1) styleMask: (unsigned)NSBorderlessWindowMask isPanel:NO backingType:CGSBackingStoreRetained];
    }
    [_fakeWindow makeTransparent];
    [_fakeWindow showWindowWithoutActivation];
    return _fakeWindow;
}

#endif

- (void)handleApplicationClose{
#ifdef WIN32
    for(int i = 0; i < _statusItems.count; i++){
        NSStatusItem *item = [_statusItems objectAtIndex:i];
        [item _removeTrayIcon];
    }
#endif
}

@end
