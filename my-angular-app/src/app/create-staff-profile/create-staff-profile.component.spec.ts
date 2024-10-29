import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CreateStaffProfileComponent } from './create-staff-profile.component';

describe('CreateStaffProfileComponent', () => {
  let component: CreateStaffProfileComponent;
  let fixture: ComponentFixture<CreateStaffProfileComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [CreateStaffProfileComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(CreateStaffProfileComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
